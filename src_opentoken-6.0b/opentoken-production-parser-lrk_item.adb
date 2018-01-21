-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2008, 2009, 2012, 2013, 2014 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
package body OpenToken.Production.Parser.LRk_Item is
   use type Ada.Strings.Unbounded.Unbounded_String;

   function Compute_Non_Terminals return Token_ID_Set
   is
      Result : Token_ID_Set;
   begin
      Result (Token.Token_ID'First .. Token.Terminal_ID'Last)                      := (others => False);
      Result (Token.Token_ID'Succ (Token.Terminal_ID'Last) .. Token.Token_ID'Last) := (others => True);
      return Result;
   end Compute_Non_Terminals;

   Non_Terminals : constant Token_ID_Set := Compute_Non_Terminals;

   function Image (Item : in Token_ID_Set) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Need_Comma then
               Result := Result & ", ";
            end if;
            Result     := Result & Token.Token_Image (I);
            Need_Comma := True;
         end if;
      end loop;
      return To_String (Result);
   end Image;

   function First_Derivations
     (Grammar              : in Production_List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Non_Terminal         : in Token.Token_ID;
      Trace                : in Boolean)
     return Token_ID_Set
   is
      use Token_List;

      Prod_Iterator  : Production_List.List_Iterator;
      Token_Iterator : List_Iterator;

      Derived_Token : Token.Token_ID;

      Derivations   : Token_ID_Set := (others => False);
      Added_Tokens  : Token_ID_Set;
      Search_Tokens : Token_ID_Set := (others => False);

   begin

      Search_Tokens (Non_Terminal) := True;

      while Search_Tokens /= Token_ID_Set'(others => False) loop

         Added_Tokens := (others => False);

         --  search all productions for rightmost derivations for
         --  tokens we found last time.
         Prod_Iterator := Production_List.Initial_Iterator (Grammar);
         while not Production_List.Past_Last (Prod_Iterator) loop
            if Search_Tokens (Token.ID (Production_List.Get_Production (Prod_Iterator).LHS.all)) then
               Token_Iterator := Initial_Iterator
                 (Production_List.Get_Production (Prod_Iterator).RHS.Tokens);

               loop
                  if Token_Iterator /= Null_Iterator then
                     Derived_Token := ID (Token_Iterator);

                     if not Derivations (Derived_Token) then
                        Added_Tokens (Derived_Token) := True;
                     end if;

                     if (Derived_Token in Nonterminal_ID and then Has_Empty_Production (Derived_Token)) and
                       Next (Token_Iterator) /= Null_Iterator
                     then
                        Token_Iterator := Next (Token_Iterator);
                     else
                        exit;
                     end if;
                  else
                     exit;
                  end if;
               end loop;
            end if;

            Production_List.Next_Production (Prod_Iterator);
         end loop;

         if Trace then
            if Added_Tokens /= Token_ID_Set'(others => False) then
               Ada.Text_IO.Put_Line (Token.Token_Image (Non_Terminal) & ": adding " & Image (Added_Tokens));
            end if;
         end if;

         Derivations   := Derivations or Added_Tokens;
         Search_Tokens := Added_Tokens and Non_Terminals;
      end loop;

      return Derivations;
   end First_Derivations;

   function First_Derivations
     (Grammar              : in Production_List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Trace                : in Boolean)
     return Derivation_Matrix
   is
      Matrix : Derivation_Matrix;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("First Derivations:");
      end if;

      for NT_Index in Matrix'Range loop
         Matrix (NT_Index) := First_Derivations (Grammar, Has_Empty_Production, NT_Index, Trace);
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Matrix;
   end First_Derivations;

   function Has_Empty_Production (Grammar : in Production_List.Instance) return Nonterminal_ID_Set
   is
      use type Token_List.List_Iterator;
      Result : Nonterminal_ID_Set := (others => False);
      Prod_I : Production_List.List_Iterator;
      Prod   : OpenToken.Production.Instance;
      RHS_I  : Token_List.List_Iterator;
   begin
      Prod_I := Production_List.Initial_Iterator (Grammar);
      while not Production_List.Past_Last (Prod_I) loop

         Prod  := Production_List.Get_Production (Prod_I);
         RHS_I := Token_List.Initial_Iterator (Prod.RHS.Tokens);

         if RHS_I = Token_List.Null_Iterator then
            Result (LHS_ID (Prod)) := True;
         end if;
         Production_List.Next_Production (Prod_I);
      end loop;
      return Result;
   end Has_Empty_Production;

   function Deep_Copy (Item : in Item_Lookahead_Ptr) return Item_Lookahead_Ptr
   is
      I      : Item_Lookahead_Ptr := Item;
      Result : Item_Lookahead_Ptr;
   begin
      while I /= null loop
         Result := new Item_Lookahead'
           (Last       => I.Last,
            Lookaheads => I.Lookaheads,
            Next       => Result);
         I := I.Next;
      end loop;
      return Result;
   end Deep_Copy;

   function Item_Node_Of
     (Prod       : in Production_List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Item_Lookahead_Ptr := null)
     return Item_Node
   is begin
      return
        (Prod       => Production_List.Get_Production (Prod),
         Dot        => Token_List.Initial_Iterator (Production_List.Get_Production (Prod).RHS.Tokens),
         State      => State,
         Lookaheads => Deep_Copy (Lookaheads),
         Next       => null);
   end Item_Node_Of;

   function Item_Node_Of
     (Prod  : in OpenToken.Production.Instance;
      State : in Unknown_State_Index)
     return Item_Node
   is begin
      return
        (Prod       => Prod,
         Dot        => Token_List.Initial_Iterator (Prod.RHS.Tokens),
         State      => State,
         Lookaheads => null,
         Next       => null);
   end Item_Node_Of;

   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead;
      Added :    out Boolean)
   is
      Found_Match : Boolean := False;
      Match_Set   : Item_Lookahead_Ptr := Set;
   begin

      --  Look for a lookahead that matches
      while Match_Set /= null loop
         if
           Match_Set.Lookaheads (1 .. Match_Set.Last) =
           Value.Lookaheads (1 .. Value.Last)
         then
            Found_Match := True;
            exit;
         end if;

         Match_Set := Match_Set.Next;
      end loop;

      --  If we didn't find one, add one
      if not Found_Match then
         Set := new Item_Lookahead'(Last       => Value.Last,
                                    Lookaheads => Value.Lookaheads,
                                    Next       => Set
                                   );
      end if;

      Added := not Found_Match;
   end Include;

   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead)
   is
      Added : Boolean;
   begin
      Include (Set => Set, Value => Value, Added => Added);
   end Include;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set)
   is begin
      Target.Set := new Item_Node'
        (Prod       => New_Item.Prod,
         Dot        => New_Item.Dot,
         State      => Target.State,
         Lookaheads => New_Item.Lookaheads,
         Next       => Target.Set);
   end Add;

   function Find
     (Left  : in Item_Node;
      Right : in Item_Set)
     return Item_Ptr
   is
      use type Token_List.List_Iterator;
      Current : Item_Ptr := Right.Set;
   begin
      while Current /= null loop
         if Left.Prod = Current.Prod and Left.Dot = Current.Dot then
            return Current;
         end if;
         Current := Current.Next;
      end loop;
      return null;
   end Find;

   function Find
     (Left  : in Item_Set;
      Right : in Item_Set_List)
   return Item_Set_Ptr
   is
      Right_Set  : Item_Set_Ptr := Right.Head;
      Right_Item : Item_Ptr;
      Left_Size  : Natural      := 0;
      Right_Size : Natural;
   begin
      --  Count the number of items in the left set
      Right_Item := Left.Set;
      while Right_Item /= null loop
         Left_Size := Left_Size + 1;

         Right_Item := Right_Item.Next;
      end loop;

      --  Go through the sets in the set list...
      while Right_Set /= null loop

         Right_Item := Right_Set.Set;
         Right_Size := 0;
         while Right_Item /= null loop

            if Find (Right_Item.all, Left) = null then
               exit;
            end if;

            Right_Size := Right_Size + 1;
            Right_Item := Right_Item.Next;
         end loop;

         if Right_Item = null and Left_Size = Right_Size then
            return Right_Set;
         end if;

         Right_Set := Right_Set.Next;
      end loop;

      return null;
   end Find;

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr
   is
      Set : Item_Set_Ptr := Sets.Head;
   begin
      while Set /= null loop
         if Set.State = State then
            return Set;
         end if;

         Set := Set.Next;
      end loop;

      return null;
   end Find;

   function Is_In
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Boolean
   is begin
      return Find (Left, Right) /= null;
   end Is_In;

   function Is_In
     (Set_Ptr   : in Item_Set_Ptr;
      Symbol    : in Token.Token_ID;
      Goto_List : in Set_Reference_Ptr)
     return Boolean
   is
      Goto_Ptr : Set_Reference_Ptr := Goto_List;
      use type Token.Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Set = Set_Ptr and Goto_Ptr.Symbol = Symbol then
            return True;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return False;
   end Is_In;

   function Goto_Set
     (From   : in Item_Set;
      Symbol : in Token.Token_ID)
     return Item_Set_Ptr
   is
      Goto_Ptr : Set_Reference_Ptr := From.Goto_List;
      use type Token.Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Symbol = Symbol then
            return Goto_Ptr.Set;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return null;
   end Goto_Set;

   function Merge
     (New_Item     : in out Item_Node;
      Existing_Set : in out Item_Set)
     return Boolean
   is
      --  Merge lookaheads of New_Item into Existing_Set. Return True
      --  if Existing_Set is modified.
      --
      --  New_Item.Lookaheads are moved or deallocated, as appropriate. Rest of New_Item is copied or deallocated.

      Found : constant Item_Ptr := Find (New_Item, Existing_Set);

      New_Lookahead      : Item_Lookahead_Ptr; --  From New_Item
      Existing_Lookahead : Item_Lookahead_Ptr; --  in Existing_Set
      Temp               : Item_Lookahead_Ptr; --  for moves
      Result_Lookahead   : Item_Lookahead_Ptr; --  add new not in existing
      Found_Match        : Boolean;
      Modified           : Boolean := False;
   begin
      if Found = null then
         Existing_Set.Set := new Item_Node'
           (Prod       => New_Item.Prod,
            Dot        => New_Item.Dot,
            State      => Unknown_State,
            Lookaheads => New_Item.Lookaheads,
            Next       => Existing_Set.Set);

         Modified := True;

      else
         --  Merge their lookaheads.
         Result_Lookahead := Found.Lookaheads;
         New_Lookahead    := New_Item.Lookaheads;

         while New_Lookahead /= null loop
            Existing_Lookahead := Found.Lookaheads;

            Found_Match := False;
            while Existing_Lookahead /= null loop

               if
                 Existing_Lookahead.Lookaheads (1 .. Existing_Lookahead.Last) =
                 New_Lookahead.Lookaheads (1 .. New_Lookahead.Last)
               then
                  Found_Match := True;
                  exit;
               end if;

               Existing_Lookahead := Existing_Lookahead.Next;
            end loop;

            if not Found_Match then
               --  New lookahead not in Existing; move New to Result
               Temp               := New_Lookahead.Next;
               New_Lookahead.Next := Result_Lookahead;
               Result_Lookahead   := New_Lookahead;
               New_Lookahead      := Temp;
               Modified           := True;
            else
               --  New lookahead in Existing; free new
               Temp               := New_Lookahead.Next;
               New_Lookahead.Next := null;
               Free (New_Lookahead);
               New_Lookahead      := Temp;
            end if;
         end loop;

         Found.Lookaheads    := Result_Lookahead;
         New_Item.Lookaheads := null;
         Free (New_Item);
      end if;
      return Modified;
   end Merge;

   function Lookahead_Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production_List.Instance;
      Trace                : in Boolean)
     return Item_Set
   is
      use type Token.Token_ID;
      use type Token_List.List_Iterator;

      Item                : Item_Ptr := Set.Set;
      Current             : Item_Ptr;
      Next_Symbol         : Token_List.List_Iterator;
      Production_Iterator : Production_List.List_Iterator;
      Result              : Item_Set;
      Merge_From          : Item_Node;
      Added_New_Item      : Boolean;
   begin
      --  Put copies of everything in Set into the closure. We don't
      --  copy Goto_List, since we are only concerned with lookaheads
      --  here.

      Result.State := Unknown_State; -- Result does _not_ match any kernel set
      while Item /= null loop
         Result.Set := new Item_Node'
           (Prod       => Item.Prod,
            Dot        => Item.Dot,
            State      => Unknown_State,
            Lookaheads => Deep_Copy (Item.Lookaheads),
            Next       => Result.Set);

         Item := Item.Next;
      end loop;

      Current        := Result.Set;
      Added_New_Item := False;
      loop
         --  If the token after Dot is a nonterminal, find its
         --  productions and place them in the set with lookaheads
         --  from the current production.
         if Current.Dot /= Token_List.Null_Iterator and then
           Token_List.ID (Current.Dot) in Nonterminal_ID
         then
            Next_Symbol := Token_List.Next_Token (Current.Dot); -- token after nonterminal, possibly null

            Production_Iterator := Production_List.Initial_Iterator (Grammar);
            while not Production_List.Past_Last (Production_Iterator) loop
               if LHS_ID (Production_List.Get_Production (Production_Iterator)) = Token_List.ID (Current.Dot) then
                  --  loop until find a terminal, or a nonterminal that cannot be empty, or end of production
                  Empty_Nonterm :
                  loop
                     if Next_Symbol = Token_List.Null_Iterator then
                        --  Need a variable, because the lookaheads might be freed.
                        Merge_From := Item_Node_Of
                          (Production_Iterator,
                           State      => Unknown_State,
                           Lookaheads => Current.Lookaheads);

                        Added_New_Item := Added_New_Item or Merge (Merge_From, Result);
                        exit Empty_Nonterm;

                     elsif Token_List.ID (Next_Symbol) in Token.Terminal_ID then
                        Merge_From := Item_Node_Of
                          (Production_Iterator,
                           State         => Unknown_State,
                           Lookaheads    => new Item_Lookahead'
                             (Last       => 1,
                              Lookaheads => (1 => Token_List.ID (Next_Symbol)),
                              Next       => null));

                        Added_New_Item := Added_New_Item or Merge (Merge_From, Result);
                        exit Empty_Nonterm;

                     else
                        --  Next_Symbol is a nonterminal
                        for Terminal in Token.Terminal_ID loop
                           if First (Token_List.ID (Next_Symbol)) (Terminal) then
                              Merge_From := Item_Node_Of
                                (Production_Iterator,
                                 State         => Unknown_State,
                                 Lookaheads    => new Item_Lookahead'
                                   (Last       => 1,
                                    Lookaheads => (1 => Terminal),
                                    Next       => null));

                              Added_New_Item := Added_New_Item or Merge (Merge_From, Result);
                           end if;
                        end loop;

                        if Has_Empty_Production (Token_List.ID (Next_Symbol)) then
                           Next_Symbol := Token_List.Next_Token (Next_Symbol);
                        else
                           exit Empty_Nonterm;
                        end if;
                     end if;
                  end loop Empty_Nonterm;

                  Next_Symbol := Token_List.Next_Token (Current.Dot);
               end if;

               Production_List.Next_Production (Production_Iterator);
            end loop;
         end if; -- Dot is is at non-terminal

         if Current.Next = null then
            exit when not Added_New_Item;

            --  This used to have logic to "only review new items",
            --  but that missed items that were modified by adding new
            --  lookaheads. We'll come back and find a better
            --  optimization if this proves too slow.
            Current        := Result.Set;
            Added_New_Item := False;

            if Trace then
               Ada.Text_IO.Put_Line ("Result:");
               Put (Result);
               Ada.Text_IO.New_Line;
            end if;
         else
            Current := Current.Next;
         end if;

      end loop;

      return Result;
   end Lookahead_Closure;

   function Goto_Transitions
     (Kernel               : in Item_Set;
      Symbol               : in Token.Token_ID;
      First                : in Derivation_Matrix;
      Grammar              : in Production_List.Instance)
     return Item_Set
   is
      use Token_List;
      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;

      Item   : Item_Ptr := Kernel.Set;
      Dot_ID : Token.Token_ID;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop

         if Item.Dot /= Null_Iterator then

            Dot_ID := ID (Item.Dot);
            --  ID of token after Dot

            if Dot_ID = Symbol then
               Goto_Set.Set := new Item_Node'
                 (Prod       => Item.Prod,
                  Dot        => Next_Token (Item.Dot),
                  State      => Unknown_State, -- replaced in LR0_Kernels
                  Lookaheads => Item.Lookaheads,
                  Next       => Goto_Set.Set);
            end if;

            if Dot_ID in Nonterminal_ID and then First (Dot_ID)(Symbol) then
               --  Find the production(s) that create Dot_ID
               --  with first token Symbol and put them in
               declare
                  Prod_I : Production_List.List_Iterator := Production_List.Initial_Iterator (Grammar);
                  Prod   : OpenToken.Production.Instance;
                  RHS_I  : List_Iterator;
               begin
                  while not Production_List.Past_Last (Prod_I) loop
                     Prod  := Production_List.Get_Production (Prod_I);
                     RHS_I := Initial_Iterator (Prod.RHS.Tokens);

                     if (Dot_ID = LHS_ID (Prod) or First (Dot_ID)(LHS_ID (Prod))) and
                       (RHS_I /= Null_Iterator and then ID (RHS_I) = Symbol)
                     then
                        declare
                           New_Item : constant Item_Node :=
                             (Prod       => Prod,
                              Dot        => Next_Token (RHS_I),
                              State      => Unknown_State, -- replaced in LR0_Kernels
                              Lookaheads => null,
                              Next       => Goto_Set.Set);
                        begin
                           if null = Find (New_Item, Goto_Set) then
                              Goto_Set.Set := new Item_Node'(New_Item);
                              --  else already in goto set
                           end if;
                        end;
                     end if;

                     Production_List.Next_Production (Prod_I);
                  end loop;
               end;
            end if;
         end if; -- item.dot /= null

         Item := Item.Next;
      end loop;

      return Goto_Set;
   end Goto_Transitions;

   procedure Free (Subject : in out Item_Node)
   is
      Lookahead : Item_Lookahead_Ptr := Subject.Lookaheads;
   begin
      while Lookahead /= null loop
         Subject.Lookaheads := Lookahead.Next;
         Free (Lookahead);
         Lookahead := Subject.Lookaheads;
      end loop;

   end Free;

   procedure Free (Subject : in out Item_Set)
   is
      Item     : Item_Ptr          := Subject.Set;
      Goto_Set : Set_Reference_Ptr := Subject.Goto_List;
   begin
      while Item /= null loop
         Subject.Set := Item.Next;
         Free (Item.all);
         Free (Item);
         Item := Subject.Set;
      end loop;

      while Goto_Set /= null loop
         Subject.Goto_List := Goto_Set.Next;
         Free (Goto_Set);
         Goto_Set := Subject.Goto_List;
      end loop;

   end Free;

   procedure Free (Subject : in out Item_Set_List)
   is
      Set : Item_Set_Ptr := Subject.Head;
   begin
      while Set /= null loop
         Subject.Head := Set.Next;

         Free (Set.all);
         Free (Set);

         Set := Subject.Head;
      end loop;
   end Free;

   function LR0_Kernels
     (Grammar           : in Production_List.Instance;
      First             : in Derivation_Matrix;
      Trace             : in Boolean;
      First_State_Index : in Unknown_State_Index)
     return Item_Set_List
   is
      Kernel_List : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of
                 (Production_List.Get_Production (Production_List.Initial_Iterator (Grammar)), First_State_Index)),
            Goto_List => null,
            State     => First_State_Index,
            Next      => null),
         Size         => 1);

      New_Items_To_Check   : Boolean      := True;
      Previous_Kernel_Head : Item_Set_Ptr := null;
      Checking_Set         : Item_Set_Ptr;
      Old_Items            : Item_Set_Ptr := null;
      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;
         Old_Items            := Previous_Kernel_Head;
         Previous_Kernel_Head := Kernel_List.Head;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= Old_Items loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (Checking_Set.all);
            end if;

            for Symbol in Token.Token_ID loop

               New_Items := Goto_Transitions (Checking_Set.all, Symbol, First, Grammar);

               --  See if any of the item sets need to be added to our list
               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, Kernel_List);
                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next  := Kernel_List.Head;
                     New_Items.State := Kernel_List.Size + First_State_Index;

                     declare
                        I : Item_Ptr := New_Items.Set;
                     begin
                        while I /= null loop
                           I.State := New_Items.State;
                           I       := I.Next;
                        end loop;
                     end;

                     if Trace then
                        Ada.Text_IO.Put ("  adding new kernel on " & Token.Token_Image (Symbol) & ": ");
                        Put (New_Items);
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Checking_Set.Goto_List := new Set_Reference'
                       (Set    => Kernel_List.Head,
                        Symbol => Symbol,
                        Next   => Checking_Set.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Set_Ptr   => New_Items_Set,
                        Symbol    => Symbol,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put ("  adding goto on " & Token.Token_Image (Symbol) & ": ");
                           Put (New_Items_Set.all);
                        end if;

                        Checking_Set.Goto_List := new Set_Reference'
                          (Set    => New_Items_Set,
                           Symbol => Symbol,
                           Next   => Checking_Set.Goto_List);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            Checking_Set := Checking_Set.Next;
         end loop;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Kernel_List;
   end LR0_Kernels;

   function Token_Name (Subject : in Token.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Subject.all));
   end Token_Name;
   function Token_Name (Subject : in Nonterminal.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Subject.all));
   end Token_Name;

   function Print (Item : in Item_Lookahead) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if Item.Last = 0 then
         return "#";
      else
         for Index in 1 .. Item.Last loop
            if Index > 1 then
               Result := Result & " ";
            end if;
            Result := Result & Token.Token_Image (Item.Lookaheads (Index));
         end loop;
      end if;

      return To_String (Result);
   end Print;

   function Print (Item : in Item_Lookahead_Ptr) return String
   is
      use Ada.Strings.Unbounded;
      Lookahead : Item_Lookahead_Ptr := Item;
      Result    : Unbounded_String := Null_Unbounded_String;
   begin
      if Lookahead = null then
         return "";
      else
         Result := Result & ", ";

         loop
            Result := Result & Print (Lookahead.all);

            Lookahead := Lookahead.Next;

            exit when Lookahead = null;

            Result := Result & "/";
         end loop;

         return To_String (Result);
      end if;
   end Print;

   function Image_Item
     (Item            : in Item_Node;
      Show_State      : in Boolean;
      Show_Lookaheads : in Boolean;
      Show_Tag        : in Boolean := False)
     return String
   is
      use Token_List;

      Token_Index : List_Iterator;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Token_Name (Item.Prod.LHS)) &
        (if Show_Tag then "(" & Ada.Tags.Expanded_Name (Item.Prod.LHS.all'Tag) & ")"
         else "") &
        " <=";

   begin
      Token_Index := Initial_Iterator (Item.Prod.RHS.Tokens);

      while Token_Index /= Null_Iterator loop
         if Token_Index = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Token_Name (Token_Handle (Token_Index));
         Next_Token (Token_Index);
      end loop;

      if Item.Dot = Null_Iterator then
         Result := Result & " ^";
      end if;

      if Show_State then
         Result := Result & " in " & Unknown_State_Index'Image (Item.State);
      end if;

      if Show_Lookaheads then
         Result := Result & Print (Item.Lookaheads);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image_Item;

   procedure Put (Item : in Item_Node; Show_Lookaheads : in Boolean) is
   begin
      Ada.Text_IO.Put (Image_Item (Item, Show_State => True, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put (Item : in Set_Reference_Ptr)
   is
      use Ada.Text_IO;
      Reference : Set_Reference_Ptr := Item;
   begin
      while Reference /= null loop
         Put_Line
           ("      on " & Token.Token_Image (Reference.Symbol) &
              " => State" & Unknown_State_Index'Image (Reference.Set.State));

         Reference := Reference.Next;
      end loop;
   end Put;

   procedure Put (Item : in Item_Set)
   is
      use Ada.Text_IO;
      Set : Item_Ptr := Item.Set;
   begin
      Put_Line ("State" & Unknown_State_Index'Image (Item.State) & ":");

      while Set /= null loop
         Put_Line ("  " & Image_Item (Set.all, Show_State => False, Show_Lookaheads => True));

         Set := Set.Next;
      end loop;
   end Put;

   procedure Put (Item : in Item_Set_List)
   is
      use Ada.Text_IO;
      Set : Item_Set_Ptr := Item.Head;
   begin
      Put_Line ("Number of Kernel Sets =" & Unknown_State_Index'Image (Item.Size));

      while Set /= null loop
         Put (Set.all);
         Put_Line ("   Goto:");
         Put (Set.Goto_List);

         Set := Set.Next;
      end loop;
   end Put;

end OpenToken.Production.Parser.LRk_Item;
