--  Copyright (C) 2002 - 2005, 2008 - 2014 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body OpenToken.Production.Parser.LALR.Generator is

   --  The following types are used for computing lookahead
   --  propagations

   type Item_List;
   type Item_List_Ptr is access Item_List;
   type Item_List is record
      Item : LRk.Item_Ptr;
      Next : Item_List_Ptr;
   end record;

   type Item_Item_List_Mapping;
   type Item_Item_List_Mapping_Ptr is access Item_Item_List_Mapping;

   type Item_Item_List_Mapping is record
      From : LRk.Item_Ptr;
      To   : Item_List_Ptr;
      Next : Item_Item_List_Mapping_Ptr;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);

   procedure Free (List : in out Item_List_Ptr)
   is
      Old_Item : Item_List_Ptr := List;
   begin
      while Old_Item /= null loop
         List := Old_Item.Next;
         Dispose (Old_Item);
         Old_Item := List;
      end loop;
   end Free;

   procedure Free (List : in out Item_Item_List_Mapping_Ptr)
   is
      Old_Mapping : Item_Item_List_Mapping_Ptr := List;
   begin
      while Old_Mapping /= null loop
         List := Old_Mapping.Next;
         Free (Old_Mapping.To);
         Dispose (Old_Mapping);
         Old_Mapping := List;
      end loop;
   end Free;

   ----------
   --  Debug output

   procedure Print_Propagations (Propagations : Item_Item_List_Mapping_Ptr) is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Propagations:");

      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LRk.Put (Next_Prop.From.all, Show_Lookaheads => False);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("To   ");
            LRk.Put (Next_To.Item.all, Show_Lookaheads => False);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Print_Propagations;

   function Image (Item : in Conflict) return String
   is begin
      return
        (Conflict_Parse_Actions'Image (Item.Action_A) & "/" &
           Conflict_Parse_Actions'Image (Item.Action_B) & " in state " &
           Token.Token_Image (Item.LHS_A) & ", " &
           Token.Token_Image (Item.LHS_B) &
           " (" & State_Index'Image (Item.State_Index) & ") on token " &
           Token.Token_Image (Item.On));
   end Image;

   procedure Put (Item : in Conflict_Lists.List)
   is begin
      for Conflict of Item loop
         Ada.Text_IO.Put_Line (Image (Conflict));
      end loop;
   end Put;

   procedure Put_Parse_Action (Action : in Parse_Action_Node_Ptr)
   is
      use Ada.Text_IO;
      Ptr    : Parse_Action_Node_Ptr   := Action;
      Column : constant Positive_Count := Col;
   begin
      loop
         Put (Ptr.Item);
         Ptr := Ptr.Next;
         exit when Ptr = null;
         Put_Line (",");
         Set_Col (Column);
      end loop;
   end Put_Parse_Action;

   procedure Put (State : in Parse_State)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use LRk;
      Action_Ptr : Action_Node_Ptr := State.Action_List;
      Goto_Ptr   : Goto_Node_Ptr   := State.Goto_List;
   begin
      if Action_Ptr = null then
         raise Programmer_Error with "LALR: Action contains no default entry";
      end if;

      while Action_Ptr /= null loop
         if Action_Ptr.Next = null then
            Put ("   default" & (Token_Image_Width - 7) * ' ' & " => ");
            Put_Parse_Action (Action_Ptr.Action);
            New_Line;
         else
            Put ("   " & Token.Token_Image (Action_Ptr.Symbol) &
                   (Token_Image_Width - Token.Token_Image (Action_Ptr.Symbol)'Length) * ' '
                   & " => ");
            Put_Parse_Action (Action_Ptr.Action);
            New_Line;
         end if;
         Action_Ptr := Action_Ptr.Next;
      end loop;

      New_Line;

      while Goto_Ptr /= null loop
         Put_Line
           ("   " & Token.Token_Image (Goto_Ptr.Symbol) &
              (Token_Image_Width - Token.Token_Image (Goto_Ptr.Symbol)'Length) * ' ' &
              " goto state" & State_Index'Image (Goto_Ptr.State));
         Goto_Ptr := Goto_Ptr.Next;
      end loop;
   end Put;

   procedure Put_Parse_Table
     (Table   : in Parse_Table_Ptr;
      Kernels : in LRk.Item_Set_List)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Parse Table:");
      for State in Table'Range loop
         LRk.Put (LRk.Find (State, Kernels).all);
         New_Line;
         Put (Table (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   ----------
   --  Generator utils

   function Find
     (Symbol      : in Token.Terminal_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr
   is
      use type Token.Terminal_ID;
      Action_Node : Action_Node_Ptr := Action_List;
   begin
      while Action_Node /= null loop
         if Action_Node.Symbol = Symbol then
            return Action_Node;
         end if;
         Action_Node := Action_Node.Next;
      end loop;

      return null;
   end Find;

   --  Add propagation entries (if they don't already exist) from From
   --  to all kernel items that match To.
   procedure Add_Propagations
     (From         : in     LRk.Item_Ptr;
      From_Set     : in     LRk.Item_Set;
      To           : in     LRk.Item_Node;
      For_Token    : in     Token.Token_ID;
      Propagations : in out Item_Item_List_Mapping_Ptr)
   is
      use type Token_List.List_Iterator;
      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;

      To_Kernel : constant LRk.Item_Ptr := LRk.Find (To, LRk.Goto_Set (From_Set, For_Token).all);

      Prop_Match    : Item_Item_List_Mapping_Ptr := Propagations;
      Prop_To_Match : Item_List_Ptr;
      Found_From    : Boolean                    := False;
      Found_To      : Boolean                    := False;
   begin
      if To_Kernel = null then
         return;
      end if;

      Find_Matching_Prop :
      while Prop_Match /= null loop
         if Prop_Match.From = From then

            Found_From    := True;
            Prop_To_Match := Prop_Match.To;
            while Prop_To_Match /= null loop

               --  ignore lookaheads in this match
               if Prop_To_Match.Item.Prod = To_Kernel.Prod and
                 Prop_To_Match.Item.Dot = To_Kernel.Dot
               then
                  Found_To := True;
                  exit Find_Matching_Prop;
               end if;
               Prop_To_Match := Prop_To_Match.Next;
            end loop;
            exit Find_Matching_Prop;
         end if;

         Prop_Match := Prop_Match.Next;
      end loop Find_Matching_Prop;

      if not Found_From then
         --  propagation for a new from_kernel
         Propagations := new Item_Item_List_Mapping'
           (From, new Item_List'(To_Kernel, Next => null), Next => Propagations);

      elsif not Found_To then
         --  add to propagations for an existing from_kernel
         Prop_Match.To := new Item_List'(To_Kernel, Next => Prop_Match.To);

      else
         raise Programmer_Error with "Add_Propagations: unexpected case";
      end if;
   end Add_Propagations;

   --  Calculate the lookaheads from Closure_Item for Source_Item.
   --  Source_Item must be one of the kernel items in Source_Set.
   --  Closure_Item must be an item in the lookahead closure of Source_Item for #.
   --
   --  Spontaneous lookaheads are put in Source_Item.Lookahead, propagated lookaheads in Propagations.
   --
   --  The start symbol (with Source_Set.Index = Accept_Index) is treated specially.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LRk.Item_Ptr;
      Source_Set   : in     LRk.Item_Set;
      Closure_Item : in     LRk.Item_Node;
      Accept_State : in     State_Index;
      Propagations : in out Item_Item_List_Mapping_Ptr;
      Used_Tokens  : in out Tokenizer.Token_Array_Boolean;
      Trace        : in     Boolean)
   is
      Spontaneous_Count : Integer := 0;

      use type LRk.Item_Lookahead_Ptr;
      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;
      use type Token_List.List_Iterator;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LRk.Put (Closure_Item, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;
      end if;

      --  If this is the start symbol production, it gets a lookahead
      --  for each terminal, so it will reduce on anything.
      if Source_Set.State = Accept_State then
         for Token_ID in Token.Terminal_ID loop
            --  These tokens are not actually used in the grammar, so
            --  we don't set Used_Tokens here.
            declare
               Lookahead : constant LRk.Item_Lookahead :=
                 (Last       => 1,
                  Lookaheads => (1 => Token_ID),
                  Next       => null);
            begin
               if Trace then
                  Ada.Text_IO.Put ("  default:");
                  LRk.Put (Source_Item.all, Show_Lookaheads => False);
                  Ada.Text_IO.Put_Line ("; " & LRk.Print (Lookahead));
               end if;

               LRk.Include (Source_Item.Lookaheads, Lookahead);
            end;
         end loop;
      end if;

      --  If the closure item doesn't have a token after Dot,
      --  there's nothing else to do.
      if Closure_Item.Dot = Token_List.Null_Iterator then
         return;
      end if;

      declare
         Token_ID   : constant Token.Token_ID           := Token_List.ID (Closure_Item.Dot);
         Next_Token : constant Token_List.List_Iterator := Token_List.Next_Token (Closure_Item.Dot);

         Next_Item : constant LRk.Item_Node :=
           (Prod       => Closure_Item.Prod,
            Dot        => Next_Token,
            State      => Unknown_State,
            Lookaheads => null,
            Next       => null);

         Next_Kernel : constant LRk.Item_Ptr  := LRk.Find (Next_Item, LRk.Goto_Set (Source_Set, Token_ID).all);
         Lookahead   : LRk.Item_Lookahead_Ptr := Closure_Item.Lookaheads;
      begin
         begin
            Used_Tokens (Token_ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Token.Token_Image (Token_ID) & " used in grammar";
         end;

         while Lookahead /= null loop
            if Lookahead.Last = 0 then
               Add_Propagations
                 (From         => Source_Item,
                  From_Set     => Source_Set,
                  To           => Next_Item,
                  For_Token    => Token_ID,
                  Propagations => Propagations);

            else
               if Next_Kernel /= null then
                  if Trace then
                     Spontaneous_Count := Spontaneous_Count + 1;
                     Ada.Text_IO.Put_Line ("  spontaneous: " & LRk.Print (Lookahead.all));
                  end if;

                  LRk.Include (Next_Kernel.Lookaheads, Lookahead.all);
               end if;

            end if;

            Lookahead := Lookahead.Next;
         end loop;

         if Spontaneous_Count > 0 then
            Ada.Text_IO.Put ("  Next_Kernel (" & Token.Token_Image (Token_ID) & "): ");
            LRk.Put (Next_Kernel.all, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List  : in Item_Item_List_Mapping_Ptr;
      Trace : in Boolean)
   is
      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LRk.Item_Lookahead_Ptr;
      Added_One     : Boolean;
      Added_Some    : Boolean := False;

      use type LRk.Item_Lookahead_Ptr;
   begin
      while More_To_Check loop

         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookaheads;

            while Lookahead /= null loop

               if Lookahead.Last > 0 then
                  To := Mapping.To;
                  while To /= null loop
                     LRk.Include (To.Item.Lookaheads, Lookahead.all, Added_One);

                     if Trace and Added_One then
                        Added_Some := True;
                        Ada.Text_IO.Put ("  to:");
                        LRk.Put (To.Item.all, Show_Lookaheads => True);
                        Ada.Text_IO.New_Line;
                     end if;

                     More_To_Check := More_To_Check or Added_One;
                     To := To.Next;
                  end loop;
               end if;

               Lookahead := Lookahead.Next;
            end loop;

            if Trace and Added_Some then
               Added_Some := False;
               Ada.Text_IO.Put ("from: ");
               LRk.Put (Mapping.From.all, Show_Lookaheads => True);
               Ada.Text_IO.New_Line;
            end if;

            Mapping := Mapping.Next;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LALR(1) lookaheads for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LALR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Kernels              : in out LRk.Item_Set_List;
      Accept_State         : in     State_Index;
      Used_Tokens          : in out Tokenizer.Token_Array_Boolean;
      Trace                : in     Boolean)
   is
      Kernel       : LRk.Item_Set_Ptr := Kernels.Head;
      Kernel_Item  : LRk.Item_Ptr;
      Closure_Item : LRk.Item_Ptr;

      Kernel_Item_Set : LRk.Item_Set :=
        (Set       => new LRk.Item_Node,
         Goto_List => null,
         State     => Unknown_State,
         Next      => null);

      --  '#' lookahead from [dragon]
      Propagate_Lookahead : constant LRk.Item_Lookahead_Ptr := new LRk.Item_Lookahead'
        (Last       => 0,
         Lookaheads => (others => Token.Terminal_ID'First),
         Next       => null);

      Closure : LRk.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookaheads := Propagate_Lookahead;

      while Kernel /= null loop
         if Trace then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LRk.Put (Kernel.all);
         end if;

         Kernel_Item := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Dot  := Kernel_Item.Dot;

            Closure := LRk.Lookahead_Closure (Kernel_Item_Set, Has_Empty_Production, First, Grammar, Trace => False);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel.all, Closure_Item.all, Accept_State, Propagation_List, Used_Tokens, Trace);

               Closure_Item := Closure_Item.Next;
            end loop;

            LRk.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Print_Propagations (Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LRk.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   function Find
     (Closure              : in LRk.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token.Token_ID;
      Has_Empty_Production : in LRk.Nonterminal_ID_Set)
     return Token.Token_ID
   is
      --  Return LHS of production that matches Action, Lookahead
      use Token_List;
      use type LRk.Item_Set;
      use type Token.Token_ID;
      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;

      Current : LRk.Item_Set := Closure;
      Item    : LRk.Item_Ptr;
   begin
      loop
         Item := Current.Set;
         loop
            exit when Item = null;
            case Action.Verb is
            when Shift =>
               if Item.Dot /= Null_Iterator and then
                 ID (Item.Dot) = Lookahead
               then
                  return LHS_ID (Item.Prod);
               end if;
            when Reduce =>
               if LHS_ID (Item.Prod) = Token.ID (Action.LHS.all) and
                 (Item.Dot = Null_Iterator or else
                    (Next_Token (Item.Dot) = Null_Iterator and
                       (ID (Item.Dot) in Nonterminal_ID and then
                          Has_Empty_Production (ID (Item.Dot)))))
               then
                  return Token.ID (Action.LHS.all);
               end if;
            when others =>
               raise Programmer_Error;
            end case;
            Item := Item.Next;
         end loop;
         exit when Current.Next = null;
         Current := Current.Next.all;
      end loop;

      Ada.Text_IO.Put_Line
        ("item for " & Parse_Action_Verbs'Image (Action.Verb) &
           (case Action.Verb is
            when Shift => State_Index'Image (Action.State),
            when Reduce => " " & Token.Token_Image (Token.ID (Action.LHS.all)),
            when others => "") & ", " &
           Token.Token_Image (Lookahead) & " not found in");
      LRk.Put (Closure);
      raise Programmer_Error;
   end Find;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean
   is
      use type Token.Token_ID;
   begin
      --  ignore State_Index
      return
        Known.Action_A = Item.Action_A and
        Known.LHS_A = Item.LHS_A and
        Known.Action_B = Item.Action_B and
        Known.LHS_B = Item.LHS_B and
        Known.On = Item.On;
   end Match;

   function Is_Present (Item : in Conflict; Conflicts : in Conflict_Lists.List) return Boolean
   is
      use Conflict_Lists;
      I : Cursor := Conflicts.First;
   begin
      loop
         exit when I = No_Element;
         if Match (Item, Conflicts.Constant_Reference (I)) then
            return True;
         end if;
         I := Next (I);
      end loop;
      return False;
   end Is_Present;

   --  Add (Symbol, Action) to Action_List
   --  Closure .. Conflicts are for conflict reporting
   procedure Add_Action
     (Symbol               : in     Token.Terminal_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LRk.Item_Set;
      State_Index          : in     Unknown_State_Index;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Trace then
         Ada.Text_IO.Put (Token.Token_Image (Symbol) & " => ");
         Put (Action);
         Ada.Text_IO.New_Line;
      end if;

      if Matching_Action /= null then
         if Matching_Action.Action.Item = Action then
            --  Matching_Action is identical to Action, so there is no
            --  conflict; just don't add it again.
            if Trace then
               Ada.Text_IO.Put_Line (" - already present");
            end if;
            return;
         else
            --  There is a conflict. Report it, but add it anyway, so
            --  an enhanced parser can follow both paths
            declare
               --  Enforce canonical Reduce/Reduce or Shift/Reduce
               --  order, to simplify searching and code generation.
               Action_A : constant Parse_Action_Rec :=
                 (if Action.Verb = Shift then Action else Matching_Action.Action.Item);

               Action_B : constant Parse_Action_Rec :=
                 (if Action.Verb = Shift then Matching_Action.Action.Item else Action);

               Action_A_Ptr : Parse_Action_Node_Ptr;
               Action_B_Ptr : Parse_Action_Node_Ptr;

               New_Conflict : constant Conflict :=
                 (Action_A    => Action_A.Verb,
                  Action_B    => Action_B.Verb,
                  LHS_A       => Find (Closure, Action_A, Symbol, Has_Empty_Production),
                  LHS_B       => Find (Closure, Action_B, Symbol, Has_Empty_Production),
                  State_Index => State_Index,
                  On          => Symbol);
            begin
               if not Is_Present (New_Conflict, Conflicts) then
                  --  The same conflict may occur in a different
                  --  kernel. Only add it to conflicts once, but still
                  --  need second action on current kernel.
                  Conflicts.Append (New_Conflict);

                  if Trace then
                     Ada.Text_IO.Put_Line (" - conflict added");
                  end if;
               else
                  if Trace then
                     Ada.Text_IO.Put_Line (" - conflict duplicate");
                  end if;
               end if;

               if Action.Verb = Shift then
                  Action_A_Ptr := new Parse_Action_Node'(Action, Matching_Action.Action);

               else
                  Action_B_Ptr      := new Parse_Action_Node'(Action, null);
                  Action_A_Ptr      := Matching_Action.Action;
                  Action_A_Ptr.Next := Action_B_Ptr;
               end if;

               Matching_Action.Action := Action_A_Ptr;

            end;
         end if;
      else
         Action_List := new Action_Node'
           (Symbol => Symbol,
            Action => new Parse_Action_Node'(Action, null),
            Next   => Action_List);
      end if;
   end Add_Action;

   procedure Add_Lookahead_Actions
     (Item                 : in     LRk.Item_Ptr;
      Kernel               : in     LRk.Item_Set_Ptr;
      Accept_State         : in     Unknown_State_Index;
      Action_List          : in out Action_Node_Ptr;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LRk.Item_Set;
      Trace                : in     Boolean)
   is
      --  Add actions for Item.Lookaheads to Action_List
      --  Item must be from Kernel.
      --  Closure must be from Lookahead_Closure (Kernel).
      --  Has_Empty_Production .. Closure used for conflict reporting.
      use type LRk.Item_Lookahead_Ptr;

      RHS_Length : constant Integer := Item.Prod.RHS.Tokens.Length;

      --  Only the start symbol kernel gets accept; the rest get
      --  reduce. See [dragon] algorithm 4.11 page 238, 4.10 page 234,
      --  except that here the augmenting production is implicit.
      Action : constant Parse_Action_Rec :=
        (if Kernel.State = Accept_State then
           (Accept_It, LHS (Item.Prod), Production.Action (Item.Prod), Index (Item.Prod), RHS_Length)
         else
            (Reduce, LHS (Item.Prod), Production.Action (Item.Prod), Index (Item.Prod), RHS_Length));

      Lookahead : LRk.Item_Lookahead_Ptr := Item.Lookaheads;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      while Lookahead /= null loop
         Add_Action
           (Symbol               => Lookahead.Lookaheads (1),
            Action               => Action,
            Action_List          => Action_List,
            State_Index          => Kernel.State,
            Closure              => Closure,
            Has_Empty_Production => Has_Empty_Production,
            Conflicts            => Conflicts,
            Trace                => Trace);
         Lookahead := Lookahead.Next;
      end loop;
   end Add_Lookahead_Actions;

   --  Add actions for Kernel to Table
   procedure Add_Actions
     (Kernel               : in     LRk.Item_Set_Ptr;
      Accept_State         : in     State_Index;
      Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      State : constant State_Index := Kernel.State;

      Closure : LRk.Item_Set := LRk.Lookahead_Closure
        (Kernel.all, Has_Empty_Production, First, Grammar, Trace => False);

      Item : LRk.Item_Ptr := Closure.Set;

      use type LRk.Item_Ptr;
      use type LRk.Set_Reference_Ptr;
      use type Token_List.List_Iterator;
      use type Token.Handle;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("adding actions for kernel" & State_Index'Image (Kernel.State));
         Ada.Text_IO.Put ("closure: ");
         LRk.Put (Closure);
         LRk.Put (Kernel.Goto_List);
      end if;

      while Item /= null loop
         if Item.Dot = Token_List.Null_Iterator then
            --  Pointer is at the end of the production; add a reduce
            --  or accept action.

            Add_Lookahead_Actions
              (Item, Kernel, Accept_State, Table (State).Action_List, Has_Empty_Production, Conflicts, Closure, Trace);

         elsif Token_List.ID (Item.Dot) in Token.Terminal_ID then
            --  Dot is before a terminal token.
            declare
               Dot_ID : constant Token.Terminal_ID := Token_List.ID (Item.Dot);
               --  ID of token after Item.Dot
            begin
               Add_Action
                 (Symbol               => Dot_ID,
                  Action               =>
                    (Verb              => Shift,
                     State             => LRk.Goto_Set (Kernel.all, Dot_ID).State),
                  Action_List          => Table (State).Action_List,
                  State_Index          => Kernel.State,
                  Closure              => Closure,
                  Has_Empty_Production => Has_Empty_Production,
                  Conflicts            => Conflicts,
                  Trace                => Trace);
            end;
         else
            --  Dot is before a non-terminal token; no action. An
            --  empty production for the non-terminal will appear in
            --  the closure, and be handled above.
            if Trace then
               Ada.Text_IO.Put_Line (Token.Token_Image (Token_List.ID (Item.Dot)) & " => no action");
            end if;
         end if;

         Item := Item.Next;
      end loop;

      --  Place a default error action at the end of every state.
      --  (it should always have at least one action already).
      --
      --  IMPROVEME: instead, optimize use of default action; compress
      --  accept, at least.
      declare
         --  The default action, when nothing else matches an input
         Default_Action : constant Action_Node :=
           --  The symbol here is actually irrelevant; it is the
           --  position as the last on a state's action list that makes
           --  it the default. It's too bad we can't extend an
           --  enumeration type to make this 'default', for viewing this
           --  list in a debugger. The various Put routines do replace
           --  this with 'default'.
           (Symbol => Token.Terminal_ID'Last,
            Action => new Parse_Action_Node'(Parse_Action_Rec'(Verb => Error), null),
            Next   => null);

         Last_Action : Action_Node_Ptr := Table (State).Action_List;
      begin
         if Last_Action = null then
            --  This happens if the first production in the grammar is
            --  not the start symbol production; that violates the
            --  assumptions Generate_Lookahead_Info makes when
            --  computing lookaheads, and Add_Actions makes
            --  when assigning accept/reduce actions.
            --
            --  It also happens when the start symbol production does
            --  not have an explicit EOF, or when there is more than
            --  one production that has the start symbol on the left
            --  hand side.
            --
            --  It also happens when the grammar is bad, for example:
            --
            --  declarations <= declarations & declaration
            --
            --  without 'declarations <= declaration'.
            --
            raise Programmer_Error with
              "Generating parser: state" & State_Index'Image (Kernel.State) &
              " has no actions; bad grammar, or " &
              "first production in grammar must be the only start symbol production, " &
              "and it must must have an explicit EOF.";
         else
            while Last_Action.Next /= null loop
               Last_Action := Last_Action.Next;
            end loop;
            Last_Action.Next := new Action_Node'(Default_Action);
         end if;
      end;

      LRk.Free (Closure);

      --  Fill in this state's Goto transitions
      declare
         Goto_Ptr : LRk.Set_Reference_Ptr := Kernel.Goto_List;
      begin
         while Goto_Ptr /= null loop
            if Goto_Ptr.Symbol in Nonterminal_ID then
               Table (State).Goto_List :=
                 new Goto_Node'
                 (Symbol => Goto_Ptr.Symbol,
                  State  => Goto_Ptr.Set.State,
                  Next   => Table (State).Goto_List);
            end if;
            Goto_Ptr := Goto_Ptr.Next;
         end loop;
      end;
   end Add_Actions;

   --  Add actions for all LRk_Kernels to Table.
   procedure Add_Actions
     (LRk_Kernels          : in     LRk.Item_Set_List;
      Accept_State         : in     State_Index;
      Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      Kernel : LRk.Item_Set_Ptr := LRk_Kernels.Head;
      use type LRk.Item_Set_Ptr;
   begin
      while Kernel /= null loop
         Add_Actions (Kernel, Accept_State, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace);
         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   procedure Delete_Known
     (Conflicts       : in out Conflict_Lists.List;
      Known_Conflicts : in out Conflict_Lists.List)
   is
      --  Delete all elements in Conflicts that match an element in
      --  Known_Conflicts. There can be more than one Conflict that
      --  match one Known_Conflict.
      use Conflict_Lists;
      Known      : Cursor  := Known_Conflicts.First;
      Next_Known : Cursor;
   begin
      --  WORKAROUND: GNAT GPL 2012 doesn't like an explicit exit in an 'of' loop
      loop
         exit when Known = No_Element;
         Next_Known := Next (Known);
         declare
            I      : Cursor  := Conflicts.First;
            Next_I : Cursor;
            Used   : Boolean := False;
         begin
            loop
               exit when I = No_Element;
               Next_I := Next (I);
               if Match (Element (Known), Conflicts.Constant_Reference (I)) then
                  Delete (Conflicts, I);
                  Used := True;
               end if;
               I := Next_I;
            end loop;

            if Used then
               Delete (Known_Conflicts, Known);
            end if;
         end;
         Known := Next_Known;
      end loop;
   end Delete_Known;

   function Generate
     (Grammar                  : in Production_List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
      use type Ada.Containers.Count_Type;

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant LRk.Nonterminal_ID_Set := LRk.Has_Empty_Production (Grammar);
      First                : constant LRk.Derivation_Matrix  := LRk.First_Derivations
        (Grammar, Has_Empty_Production, Trace);
      Used_Tokens          : Tokenizer.Token_Array_Boolean   := (others => False);

      Kernels : LRk.Item_Set_List := LRk.LR0_Kernels (Grammar, First, Trace, Unknown_State_Index (First_State_Index));

      I             : LRk.Item_Set_Ptr    := Kernels.Head;
      Accept_State  : Unknown_State_Index := Unknown_State;
      Unused_Tokens : Boolean             := False;

      First_Production : OpenToken.Production.Instance renames
        Production_List.Get_Production (Production_List.Initial_Iterator (Grammar));

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

      use type LRk.Item_Set_Ptr;
   begin
      Used_Tokens (LHS_ID (First_Production)) := True;

      Kernels := LRk.LR0_Kernels (Grammar, First, Trace, State_Index (First_State_Index));
      I       := Kernels.Head;

      --  Accept_State identifies the kernel that is the start symbol
      --  production, which must be the first production in Grammar.
      --  That does not guarrantee its position in Kernels, so we
      --  search for it.
      loop
         exit when I = null;
         if I.Set.Prod = First_Production then
            Accept_State := I.State;
            exit;
         end if;
         I := I.Next;
      end loop;

      if Accept_State = Unknown_State then
         raise Programmer_Error with "Accept_State = 0; something wrong with Grammar?";
      end if;

      if Trace then
         Ada.Text_IO.Put_Line ("Accept_State:" & State_Index'Image (Accept_State));
      end if;

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Accept_State, Used_Tokens, Trace);

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Token.Token_Image (I));
         end if;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LRk.Put (Kernels);
      end if;

      Table := new Parse_Table (State_Index'First .. Kernels.Size - 1 + State_Index'First);

      Add_Actions
        (Kernels, Accept_State, Grammar, Has_Empty_Production, First, Unknown_Conflicts, Table.all, Trace);

      if Put_Parse_Table then
         Generator.Put_Parse_Table (Table, Kernels);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line ("unknown conflicts:");
         Put (Unknown_Conflicts);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "unknown conflicts; aborting";
         end if;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line ("excess known conflicts:");
         Put (Known_Conflicts_Edit);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "excess known conflicts; aborting";
         end if;
      end if;

      if Unused_Tokens and not (Trace or Ignore_Unused_Tokens) then
         raise Grammar_Error with "unused tokens; aborting";
      end if;

      return Table;
   end Generate;

end OpenToken.Production.Parser.LALR.Generator;
