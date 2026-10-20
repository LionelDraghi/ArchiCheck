--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014-2015  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Characters.Handling;
with Ada.Text_IO;
package body OpenToken.Production.Parser.LALR.Parser_Lists is

   function Initialize return List
   is begin
      return
        (Parser_Label       => First_Parser_Label,
         Head               => new Parser_Node'
           (Item            =>
              (Label        => First_Parser_Label,
               Verb         => Parse_Action_Verbs'First,
               Stack        => new Stack_Node'
                 (Item      =>
                    (State  => State_Index'First,
                     Token  => null),
                  Next      => null),
               Action_Token => (null, null)),
            Next            => null,
            Prev            => null),
         Parser_Free        => null,
         Stack_Free         => null,
         Action_Token_Free  => null,
         Count              => 1);

   end Initialize;

   function Count (List : in Parser_Lists.List) return Integer
   is begin
      return List.Count;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      --  WORKAROUND: with 'Access, Debian gnat 4.9.2 reports
      --  "non-local pointer cannot point to local object", even
      --  though GNAT Pro 7.3.1 and GNAT GPL 2014 allow 'Access There
      --  doesn't seem to be a way to use a legitimate access param
      --  while still meeting the Iterator requirements.
      return (List'Unchecked_Access, Ptr => List.Head);
   end First;

   procedure Next (Cursor : in out Parser_Lists.Cursor)
   is begin
      if Cursor.Ptr /= null then
         Cursor.Ptr := Cursor.Ptr.Next;
      end if;
   end Next;

   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr = null;
   end Is_Done;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Cursor.List.Count;
   end Active_Parser_Count;

   function Label (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Cursor.Ptr.Item.Label;
   end Label;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs)
   is begin
      Cursor.Ptr.Item.Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Ptr.Item.Verb;
   end Verb;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Item.Stack = null;
   end Stack_Empty;

   function Peek (Cursor : in Parser_Lists.Cursor) return Stack_Item
   is begin
      return Cursor.Ptr.Item.Stack.Item;
   end Peek;

   procedure Free (List : in out Parser_Lists.List; Stack : in out Stack_Node_Access)
   is
      Temp_Free : constant Stack_Node_Access := List.Stack_Free;
   begin
      List.Stack_Free := Stack;
      Stack           := Stack.Next; -- not null; for free (cursor) below

      List.Stack_Free.all :=
        (Item     =>
           (State => Unknown_State,
            Token => null), -- Token is free'd after being passed to user action. FIXME: say what?
         Next     => Temp_Free);
   end Free;

   function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item
   is
      Result : constant Stack_Item := Cursor.Ptr.Item.Stack.Item;
   begin
      Free (Cursor.List.all, Cursor.Ptr.Item.Stack);

      return Result;
   end Pop;

   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item)
   is
      Temp : constant Stack_Node_Access := Cursor.List.Stack_Free;
   begin
      if Temp = null then
         Cursor.Ptr.Item.Stack := new Stack_Node'(Item, Cursor.Ptr.Item.Stack);
      else
         Cursor.List.Stack_Free := Cursor.List.Stack_Free.Next;
         Temp.all               := (Item, Cursor.Ptr.Item.Stack);
         Cursor.Ptr.Item.Stack  := Temp;
      end if;
   end Push;

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean
   is
      use type Token.Handle;
      use type Token.Token_ID;

      Stack_1 : Stack_Node_Access := Cursor_1.Ptr.Item.Stack;
      Stack_2 : Stack_Node_Access := Cursor_2.Ptr.Item.Stack;
   begin
      loop
         exit when Stack_1 = null or Stack_2 = null;
         if not
           (Stack_1.Item.State = Stack_2.Item.State and
              ((Stack_1.Item.Token = null or Stack_2.Item.Token = null) or else
                 Stack_1.Item.Token.ID = Stack_2.Item.Token.ID))
         then
            return False;
         end if;
         Stack_1 := Stack_1.Next;
         Stack_2 := Stack_2.Next;
      end loop;
      return Stack_1 = null and Stack_2 = null;
   end Stack_Equal;

   procedure Put_Top_10 (Cursor : in Parser_Lists.Cursor)
   is
      use Ada.Text_IO;
      use type Token.Handle;
      Stack_I : Stack_Node_Access := Cursor.Ptr.Item.Stack;
   begin
      Put (Integer'Image (Cursor.Ptr.Item.Label) & " stack: ");
      for I in 1 .. 10 loop
         exit when Stack_I = null;
         Put
           (State_Index'Image (Stack_I.Item.State) & " : " &
              (if Stack_I.Item.Token = null then ""
               else Stack_I.Item.Token.Image) &
              ", ");
         Stack_I := Stack_I.Next;
      end loop;
      New_Line;
   end Put_Top_10;

   function Action_Token_Count (Cursor : in Parser_Lists.Cursor) return Integer
   is
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Action_Token.Head;
      Result       : Integer                  := 0;
   begin
      loop
         exit when Action_Token = null;
         Result := Result + 1;
         Action_Token := Action_Token.Next;
      end loop;
      return Result;
   end Action_Token_Count;

   procedure Enqueue
     (List              : in out Action_Token_List;
      Action_Token_Free : in out Action_Token_Node_Access;
      Action_Token      : in     Parser_Lists.Action_Token)
   is
      Temp : constant Action_Token_Node_Access := Action_Token_Free;

      Node : constant Action_Token_Node := (Action_Token, null, List.Tail);
   begin
      if Temp = null then
         if List.Tail = null then
            List.Tail := new Action_Token_Node'(Node);
            List.Head := List.Tail;
         else
            List.Tail := new Action_Token_Node'(Node);
         end if;
      else
         Action_Token_Free := Action_Token_Free.Next;
         Temp.all          := Node;
         if List.Tail = null then
            List.Tail := Temp;
            List.Head := List.Tail;
         else
            List.Tail := Temp;
         end if;
      end if;

      if List.Tail.Prev /= null then
         List.Tail.Prev.Next := List.Tail;
      end if;
   end Enqueue;

   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token)
   is begin
      Enqueue (Cursor.Ptr.Item.Action_Token, Cursor.List.Action_Token_Free, Action_Token);
   end Enqueue;

   procedure Free (List : in out Parser_Lists.List; Action_Token : in out Action_Token_Node_Access)
   is
      Temp_Free : constant Action_Token_Node_Access := List.Action_Token_Free;
   begin
      List.Action_Token_Free := Action_Token;
      Action_Token           := Action_Token.Next; -- not null; for free (cursor) below

      List.Action_Token_Free.all :=
        (Item         => Null_Action_Token, -- New_Token, Tokens are free'd after being passed to user action
         Next         => Temp_Free,
         Prev         => null);
   end Free;

   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token
   is
      Result : constant Action_Token := Cursor.Ptr.Item.Action_Token.Head.Item;
   begin
      Free (Cursor.List.all, Cursor.Ptr.Item.Action_Token.Head);

      if Cursor.Ptr.Item.Action_Token.Head = null then
         Cursor.Ptr.Item.Action_Token.Tail := null;
      end if;

      return Result;
   end Dequeue;

   function Action_Tokens_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Item.Action_Token.Head = null;
   end Action_Tokens_Empty;

   procedure Deep_Copy
     (Stack             : in     Stack_Node_Access;
      Stack_Free        : in out Stack_Node_Access;
      Action_Token      : in     Action_Token_List;
      Action_Token_Free : in out Action_Token_Node_Access;
      New_Stack         :    out Stack_Node_Access;
      New_Action_Tokens :    out Action_Token_List)
   is
      use Token_List;
      use type Token.Handle;

      --  All Action_Token.New_Token must point either to a token on
      --  Stack or to tokens in later Action_Token.Tokens; preserve
      --  that in the new copy.
      --
      --  1) Create a map of old action.new_token => new action.new_tokens
      --  2) Create new Stack, using new action.New_Token pointers or new tokens
      --  3) Create new Action, using new action.New_Token pointers or new tokens

      type New_Token_Item is record
         Old_Pointer : Token.Handle;
         New_Pointer : Token.Handle;
      end record;

      New_Token_Items : array (Integer range 1 .. Count (Action_Token)) of New_Token_Item;

      J          : Action_Token_Node_Access := Action_Token.Head;
      Action_Pos : Integer := New_Token_Items'Last;
      Iter       : List_Iterator;
      New_Tokens : Token_List.Instance;
      New_Token  : Token.Handle;

      I    : Stack_Node_Access := Stack;
      Copy : Stack_Node_Access;
      Temp : Stack_Node_Access;

      New_Stack_Item   : Stack_Item;
      Stack_Item_Found : Boolean;
   begin

      for K in New_Token_Items'Range loop
         New_Token_Items (K).Old_Pointer := Token.Handle (J.Item.New_Token);
         New_Token_Items (K).New_Pointer := Token.Copy (New_Token_Items (K).Old_Pointer);

         J := J.Next;
      end loop;

      --  Create a copy of Stack in Copy, in reverse order, using
      --  New_Token_Items. New_Token_Items appear on the stack in
      --  reverse order; not all are on stack (some are in later
      --  actions).
      loop
         exit when I = null;

         Stack_Item_Found := False;
         for K in reverse New_Token_Items'First .. Action_Pos loop
            if I.Item.Token = New_Token_Items (K).Old_Pointer then
               New_Stack_Item   := (I.Item.State, New_Token_Items (K).New_Pointer);
               Stack_Item_Found := True;
               Action_Pos       := K - 1;
            end if;
         end loop;
         if not Stack_Item_Found then
            New_Stack_Item := (I.Item.State, Token.Copy (I.Item.Token));
         end if;

         if Stack_Free = null then
            Copy := new Stack_Node'(New_Stack_Item, Copy);
         else
            Temp       := Copy;
            Copy       := Stack_Free;
            Stack_Free := Stack_Free.Next;
            Copy.all   := (New_Stack_Item, Temp);
         end if;
         I := I.Next;
      end loop;

      --  Move to New_Stack, in correct order.
      I         := Copy;
      New_Stack := null;

      loop
         exit when I = null;
         Temp      := I.Next;
         I.Next    := New_Stack;
         New_Stack := I;
         I         := Temp;
      end loop;

      --  Copy Action_Tokens, using New_Token_Items.New_Token from previous actions
      J          := Action_Token.Head;
      Action_Pos := 1;
      loop
         exit when J = null;

         Iter       := Initial_Iterator (J.Item.Tokens);
         New_Tokens := Null_List;
         loop
            exit when Iter = Null_Iterator;

            New_Token := null;
            Find_New_Token :
            for K in New_Token_Items'First .. Action_Pos - 1 loop
               if Token_Handle (Iter) = New_Token_Items (K).Old_Pointer then
                  New_Token := New_Token_Items (K).New_Pointer;
                  exit Find_New_Token;
               end if;
            end loop Find_New_Token;

            if New_Token = null then
               Append (New_Tokens, Token.Copy (Token_Handle (Iter)));
            else
               Append (New_Tokens, New_Token);
            end if;

            Next (Iter);
         end loop;

         Enqueue
           (List              => New_Action_Tokens,
            Action_Token_Free => Action_Token_Free,
            Action_Token      => (J.Item.Action,
                                  Nonterminal.Handle (New_Token_Items (Action_Pos).New_Pointer),
                                  New_Tokens));

         J          := J.Next;
         Action_Pos := Action_Pos + 1;
      end loop;
   end Deep_Copy;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class)
   is
      Temp : constant Parser_Node_Access := List.Parser_Free;

      New_Stack        : Stack_Node_Access;
      New_Action_Token : Action_Token_List;

      New_Parser : Parser_Node;
   begin
      Deep_Copy
        (Cursor.Ptr.Item.Stack, List.Stack_Free, Cursor.Ptr.Item.Action_Token, List.Action_Token_Free,
         New_Stack, New_Action_Token);

      New_Parser :=
        (Item =>
           (List.Parser_Label + 1,
            Cursor.Ptr.Item.Verb,
            New_Stack,
            New_Action_Token),
         Next => List.Head,
         Prev => null);

      List.Parser_Label := List.Parser_Label + 1;
      List.Count        := List.Count + 1;

      if Temp = null then
         List.Head := new Parser_Node'(New_Parser);

         List.Head.Next.Prev := List.Head;
      else
         List.Parser_Free    := List.Parser_Free.Next;
         Temp.all            := New_Parser;
         List.Head           := Temp;
         List.Head.Next.Prev := List.Head;
      end if;
   end Prepend_Copy;

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
   is
      Temp_Free    : constant Parser_Node_Access := Cursor.List.Parser_Free;
      Stack        : Stack_Node_Access           := Cursor.Ptr.Item.Stack;
      Action_Token : Action_Token_Node_Access    := Cursor.Ptr.Item.Action_Token.Head;
   begin
      Cursor.List.Count := Cursor.List.Count - 1;

      if Cursor.List.Head = Cursor.Ptr then
         Cursor.List.Head := Cursor.Ptr.Next;
      end if;

      if Cursor.Ptr.Prev /= null then
         Cursor.Ptr.Prev.Next := Cursor.Ptr.Next;
      end if;

      if Cursor.Ptr.Next /= null then
         Cursor.Ptr.Next.Prev := Cursor.Ptr.Prev;
      end if;

      Cursor.List.Parser_Free := Cursor.Ptr;
      Cursor.Ptr              := Cursor.Ptr.Next;

      Cursor.List.Parser_Free.Next := Temp_Free;
      Cursor.List.Parser_Free.Prev := null;

      loop
         exit when Stack = null;
         Free (Cursor.List.all, Stack);
      end loop;

      loop
         exit when Action_Token = null;
         Free (Cursor.List.all, Action_Token);
      end loop;
   end Free;

   ----------
   --  stuff for iterators

   function To_Cursor
     (List : aliased in out Parser_Lists.List'Class;
      Ptr  :         in     Parser_Node_Access)
     return Cursor
   is begin
      --  see WORKAROUND in First
      return (List'Unchecked_Access, Ptr);
   end To_Cursor;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  : in Parser_Node_Access)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
      Result : constant Constant_Reference_Type
        := (Element => Position.Item'Access);
   begin
      return Result;
   end Constant_Reference;

   type List_Access_Constant is access constant List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access_Constant;
   end record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return Object.Container.Head;
   end First;

   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
     return Parser_Node_Access
   is
      pragma Unreferenced (Object);
   begin
      if Position = null then
         return null;
      else
         return Position.Next;
      end if;
   end Next;

   function Has_Element (Cursor : in Parser_Node_Access) return Boolean
   is begin
      return Cursor /= null;
   end Has_Element;

   function Verb (Cursor : in Parser_Node_Access) return Parse_Action_Verbs
   is begin
      return Cursor.Item.Verb;
   end Verb;

   function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      --  see WORKAROUND in First
      return Iterator'(Container => Container'Unchecked_Access);
   end Iterate;

   function Count (Action_Token : in Action_Token_List) return Integer
   is
      Result : Integer := 0;
      I : Action_Token_Node_Access := Action_Token.Head;
   begin
      loop
         exit when I = null;
         Result := Result + 1;
         I      := I.Next;
      end loop;
      return Result;
   end Count;

   ----------
   --  For unit tests

   function Parser_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Parser_Node_Access := List.Parser_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Parser_Free_Count;

   function Stack_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Stack_Node_Access := List.Stack_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Stack_Free_Count;

   function Action_Token_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Action_Token_Node_Access := List.Action_Token_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Action_Token_Free_Count;

   function Is_In (Item : in Nonterminal.Handle; Stack : in Stack_Node_Access) return Boolean
   is
      use type Token.Handle;
      Stack_Node : Stack_Node_Access := Stack;
   begin
      loop
         exit when Stack_Node = null;
         if Stack_Node.Item.Token = Token.Handle (Item) then
            return True;
         end if;
         Stack_Node := Stack_Node.Next;
      end loop;
      return False;
   end Is_In;

   function Is_In (Item : in Nonterminal.Handle; Tokens : Token_List.Instance) return Boolean
   is
      use type Token.Handle;
      use Token_List;
      Iter : List_Iterator := Initial_Iterator (Tokens);
   begin
      loop
         exit when Iter = Null_Iterator;

         if Token.Handle (Item) = Token_Handle (Iter) then
            return True;
         end if;
         Next (Iter);
      end loop;
      return False;
   end Is_In;

   function Is_In_Later_Tokens
     (Token        : in Nonterminal.Handle;
      Action_Token : in Action_Token_Node_Access)
     return Boolean
   is
      Iter : Action_Token_Node_Access := Action_Token;
   begin
      loop
         exit when Iter = null;
         if Is_In (Token, Iter.Item.Tokens) then
            return True;
         end if;
         Iter := Iter.Next;
      end loop;
      return False;
   end Is_In_Later_Tokens;

   function Is_In_Prev_New_Token
     (Item         : in Token.Handle;
      Action_Token : in Action_Token_Node_Access)
     return Boolean
   is
      use type Token.Handle;
      Iter : Action_Token_Node_Access := Action_Token;
   begin
      loop
         exit when Iter = null;
         if Item = Token.Handle (Iter.Item.New_Token) then
            return True;
         end if;
         Iter := Iter.Prev;
      end loop;
      return False;
   end Is_In_Prev_New_Token;

   procedure Check_Are_In_Prev_New_Token
     (Label        : in String;
      Tokens       : in Token_List.Instance;
      Action_Token : in Action_Token_Node_Access)
   is
      use Token_List;
      Iter : List_Iterator := Initial_Iterator (Tokens);
   begin
      loop
         exit when Iter = Null_Iterator;
         if Token_Handle (Iter).all in Nonterminal.Instance and then
           (not Is_In_Prev_New_Token (Token_Handle (Iter), Action_Token.Prev))
         then
            raise Programmer_Error with Label &
              " - action token " & Token_Handle (Iter).Image & " not in prev actions tokens";
         end if;
         Next (Iter);
      end loop;
   end Check_Are_In_Prev_New_Token;

   procedure Check_Action_Stack
     (Label  : in String;
      Cursor : in Parser_Lists.Cursor)
   is
      use type Token.Handle;
      Stack        : Stack_Node_Access        := Cursor.Ptr.Item.Stack;
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Action_Token.Head;
   begin
      loop
         exit when Action_Token = null;
         if not (Is_In_Later_Tokens (Action_Token.Item.New_Token, Action_Token.Next) or
                   Is_In (Action_Token.Item.New_Token, Stack))
         then
            raise Programmer_Error with Label &
              " - action.new_token " & Token.Handle (Action_Token.Item.New_Token).Image &
              " not in later action tokens or stack";
         end if;
         Action_Token := Action_Token.Next;
      end loop;

      Action_Token := Cursor.Ptr.Item.Action_Token.Tail;
      loop
         exit when Stack = null;
         --  last item on stack has no token
         if Stack.Item.Token /= null and then Stack.Item.Token.all in Nonterminal.Instance and then
           not Is_In_Prev_New_Token (Stack.Item.Token, Action_Token)
         then
            raise Programmer_Error with Label & " - stack " & Stack.Item.Token.Image &
              " not in action.new_token";
         end if;
         Stack := Stack.Next;
      end loop;

      loop
         exit when Action_Token = null;
         Check_Are_In_Prev_New_Token (Label, Action_Token.Item.Tokens, Action_Token);
         Action_Token := Action_Token.Prev;
      end loop;

   end Check_Action_Stack;

   procedure Put (Action_Token : in Parser_Lists.Action_Token)
   is
      use type Nonterminal.Handle;
      use Ada.Characters.Handling;
      Action_Name : constant String := To_Lower
        (Token.Token_ID'Image
           --  LHS is null in unit tests
           ((if Action_Token.Action.LHS = null then Action_Token.New_Token.ID else Action_Token.Action.LHS.ID))) &
        "_" & OpenToken.Int_Image (Action_Token.Action.Index);
   begin
      Ada.Text_IO.Put (Action_Name & ": " & Action_Token.New_Token.Image & " ");
      Token_List.Print (Action_Token.Tokens);
   end Put;

   procedure Put_Action_Tokens (Cursor : in Parser_Lists.Cursor)
   is
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Action_Token.Head;
   begin
      loop
         exit when Action_Token = null;
         Put (Action_Token.Item);
         Ada.Text_IO.New_Line;
         Action_Token := Action_Token.Next;
      end loop;
   end Put_Action_Tokens;

end OpenToken.Production.Parser.LALR.Parser_Lists;
