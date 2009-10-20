-------------------------------------------------------------------------------
--
-- Copyright (C) 2002 - 2005, 2008, 2009 Stephe Leake
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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;

with OpenToken.Production.Parser.LRk_Item;

use type Ada.Strings.Unbounded.Unbounded_String;
package body OpenToken.Production.Parser.LALR is

   Line_End : constant String := "" & Ada.Characters.Latin_1.LF;

   package LRk is new OpenToken.Production.Parser.LRk_Item (1);

   --------------------------------------------------------------------------
   --  Following are the types used in the parse "table". The parse
   --  table is an array indexed by parse state that where each state
   --  contains a list of parse actions and a list of reduction
   --  actions.
   --
   --  Parse actions are indexed by the terminal they match and are either
   --    o Shift and change to a designated state.
   --    o Reduce by the given production
   --
   --  Reduction actions are indexd by the nonterminal they match and
   --  designate the state the parser need to change to.

   type Parse_Action_Verbs is (Shift, Reduce, Accept_It, Error);
   type Parse_Action (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
         when Shift =>
            State : State_Index;
         when Reduce | Accept_It =>
            Production : OpenToken.Production.Instance;
            Length     : Natural;
         when Error =>
            null;
      end case;
   end record;

   type Action_Node is record
      Symbol : Tokenizer.Terminal_ID;
      Action : Parse_Action;
      Next   : Action_Node_Ptr;
   end record;

   type Reduction_Node is record
      Symbol : Token.Token_ID;
      State  : State_Index;
      Next   : Reduction_Node_Ptr;
   end record;

   --------------------------------------------------------------------------
   --  The following types are used for the Parser's stack. The stack
   --  designates the tokens that have been read or derived, and the
   --  parser states in which that occurred.


   type State_Node;
   type State_Node_Ptr is access State_Node;

   type State_Node is record
      State      : State_Index := 0;
      Seen_Token : Token.Handle;
      Next       : State_Node_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (State_Node, State_Node_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Token.Class, Token.Handle);


   --------------------------------------------------------------------------
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

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);


   ----------------------------------------------------------------------------
   --  Free the storage allocated by this package for the given item list.
   ----------------------------------------------------------------------------
   procedure Free (List : in out Item_List_Ptr) is
      Old_Item : Item_List_Ptr := List;
   begin
      while Old_Item /= null loop
         List := Old_Item.Next;
         Dispose (Old_Item);
         Old_Item := List;
      end loop;
   end Free;

   --------------------------------------------------------------------------
   --  Free the storage allocated by this package for the given item
   --  item list mapping list.
   --------------------------------------------------------------------------
   procedure Free (List : in out Item_Item_List_Mapping_Ptr) is
      Old_Mapping : Item_Item_List_Mapping_Ptr := List;
   begin
      while Old_Mapping /= null loop
         List := Old_Mapping.Next;
         Free (Old_Mapping.To);
         Dispose (Old_Mapping);
         Old_Mapping := List;
      end loop;
   end Free;

   ----------------------------------------------------------------------------
   --  Return the action for the given state index and terminal ID.
   --  The final node for a state is assumed to match all inputs.
   ----------------------------------------------------------------------------
   function Action_For (Table : in Parse_Table_Ptr;
                        State : in State_Index;
                        ID    : in Tokenizer.Terminal_ID
                       ) return Parse_Action is

      use type Tokenizer.Terminal_ID;
      Action_Node : Action_Node_Ptr := Table.all (State).Action_List;
   begin
      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      return Action_Node.Action;
   end Action_For;

   ----------------------------------------------------------------------------
   --  Return the action for the given state index and terminal ID.
   --  The final node for a state is assumed to match all inputs.
   ----------------------------------------------------------------------------
   function Goto_For (Table : in Parse_Table_Ptr;
                      State : in State_Index;
                      ID    : in Token.Token_ID
                     ) return State_Index is

      use type Tokenizer.Terminal_ID;
      Reduction_Node : Reduction_Node_Ptr := Table.all (State).Reduction_List;
   begin
      while Reduction_Node.Next /= null and Reduction_Node.Symbol /= ID loop
         Reduction_Node := Reduction_Node.Next;
      end loop;

      return Reduction_Node.State;
   end Goto_For;

   ----------------------------------------------------------------------------
   --  Locate the action node for the non-terminal ID in the given table.
   ----------------------------------------------------------------------------
   function Find (Symbol      : in Tokenizer.Terminal_ID;
                  Action_List : in Action_Node_Ptr
                 ) return Action_Node_Ptr is

      use type Tokenizer.Terminal_ID;
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

   ----------------------------------------------------------------------------
   --  Display the given propagations. This routine is included mainly as a
   --  debugging aid.
   ----------------------------------------------------------------------------
   procedure Print_Propagations (Propagations : Item_Item_List_Mapping_Ptr) is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      Ada.Text_IO.Put_Line ("Propagations:");

      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LRk.Print_Item (Next_Prop.From.all);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("          To ");
            LRk.Print_Item (Next_To.Item.all);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Print_Propagations;

   ----------------------------------------------------------------------------
   --  Add propagation entires (if they don't already exist) from the given item
   --  to all kernel items that match the given To item.
   ----------------------------------------------------------------------------
   procedure Add_Propagations
     (From         : in     LRk.Item_Ptr;
      From_Set     : in     LRk.Item_Set;
      To           : in     LRk.Item_Node;
      For_Token    : in     Token.Token_ID;
      Kernels      : in     LRk.Item_Set_List;
      Propagations : in out Item_Item_List_Mapping_Ptr
     ) is

      To_Kernel  : LRk.Item_Ptr;

      Prop_Match    : Item_Item_List_Mapping_Ptr;
      Prop_To_Match : Item_List_Ptr;
      Found_From    : Boolean;
      Found_To      : Boolean;

      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
   begin

      --  For the kernel element that matches the given item in the goto for the
      --  given from set on the given token...
      To_Kernel := LRk.Find
        (Left  => To,
         Right => LRk.Goto_Set
         (From   => From_Set,
          Symbol => For_Token
          ).all
         );

      if To_Kernel /= null then

         -----------------------------------------------------------
         --  If there isn't already a lookahead mapping for that item
         --  and the source item, make one

         --  Look through all the propagations...
         Found_From := False;
         Prop_Match := Propagations;

         Find_Matching_Prop :
         while Prop_Match /= null loop
            if Prop_Match.From = From then

               --  Look through all the propagation mappings...
               Found_To    := True;
               Prop_To_Match := Prop_Match.To;
               while Prop_To_Match /= null loop

                  if Prop_To_Match.Item = To_Kernel then
                     Found_To := True;
                     exit Find_Matching_Prop;
                  end if;
                  Prop_To_Match := Prop_To_Match.Next;
               end loop;
            end if;

            Prop_Match := Prop_Match.Next;
         end loop Find_Matching_Prop;

         if not Found_From then

            Propagations := new Item_Item_List_Mapping'(From => From,
                                                        To   => new Item_List'
                                                        (Item => To_Kernel,
                                                         Next => null
                                                         ),
                                                        Next => Propagations
                                                        );
         elsif not Found_To then
            Prop_Match.To := new Item_List'(Item => To_Kernel,
                                            Next => Prop_Match.To
                                            );
         end if;
      end if;
   end Add_Propagations;

   --------------------------------------------------------------------------
   --  For the given source and closure item, calculate the
   --  lookaheads. If it is a spontanious lookahead, put it in the
   --  source item's lookahead. If it is a propagated lookahead, put
   --  the appropriate entry on the propagation list.
   --------------------------------------------------------------------------
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LRk.Item_Ptr;
      Source_Set   : in     LRk.Item_Set;
      Closure_Item : in     LRk.Item_Node;
      Grammar      : in     Production_List.Instance;
      Kernels      : in     LRk.Item_Set_List;
      Propagations : in out Item_Item_List_Mapping_Ptr;
      Trace        : in     Boolean)
   is

      Next_Item   : LRk.Item_Node;
      Next_Token  : Token_List.List_Iterator;
      Next_Kernel : LRk.Item_Ptr;
      Lookahead   : LRk.Item_Lookahead_Ptr := Closure_Item.Lookahead_Set;

      use type Token.Handle;
      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
      use type LRk.Item_Lookahead_Ptr;
   begin

      --  Verify that the closure item has a token past the pointer
      if Token_List.Token_Handle (Closure_Item.Pointer) = null then
         return;
      end if;

      Next_Token := Closure_Item.Pointer;
      Token_List.Next_Token (Next_Token); --  First token after pointer
      Next_Item :=
        (Prod          => Closure_Item.Prod,
         Pointer       => Next_Token,
         Lookahead_Set => null,
         Next          => null
         );

      --  If this is the first production with the pointer on the last
      --  item, it gets a lookahead for each terminal. (so it will
      --  reduce on anything).
      if Source_Set.Index = 1 and Token_List.Token_Handle (Next_Token) = null then
         for Token_ID in Tokenizer.Terminal_ID loop
            declare
               Lookahead : constant LRk.Item_Lookahead :=
                 (Last       => 1,
                  Lookaheads => (1 => Token_ID),
                  Next       => null);
            begin
               if Trace then
                  Ada.Text_IO.Put_Line ("Adding default lookahead:");
                  LRk.Print_Item (Source_Item.all);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead));
               end if;

               LRk.Include
                 (Set   => Source_Item.Lookahead_Set,
                  Value => Lookahead);
            end;
         end loop;
      end if;

      --  Check all of the closure item's lookaheads
      while Lookahead /= null loop
         if Lookahead.Last = 0 then
            --  Lookaheads propagate
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To           => Next_Item,
               For_Token    => Token.ID (Token_List.Token_Handle (Closure_Item.Pointer).all),
               Kernels      => Kernels,
               Propagations => Propagations
               );

         else
            --  Lookaheads are generated spontaneously for all items
            --  in the source item's goto for the current symbol that
            --  match the next_item.

            Next_Kernel := LRk.Find
              (Left  => Next_Item,
               Right => LRk.Goto_Set
                 (From   => Source_Set,
                  Symbol => Token.ID (Token_List.Token_Handle (Closure_Item.Pointer).all)
                 ).all
              );

            if Next_Kernel /= null then
               if Trace then
                  Ada.Text_IO.Put_Line ("Adding spontaneous lookahead:");
                  LRk.Print_Item (Next_Kernel.all);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead.all));
               end if;

               LRk.Include
                 (Set   => Next_Kernel.Lookahead_Set,
                  Value => Lookahead.all
                  );
            end if;

         end if;

         Lookahead := Lookahead.Next;
      end loop;
   end Generate_Lookahead_Info;

   --------------------------------------------------------------------------
   --  Propagate lookaheads as directed by the given propagation list,
   --  until no more lookaheads are propagated.
   --------------------------------------------------------------------------
   procedure Propagate_Lookaheads
     (List  : in Item_Item_List_Mapping_Ptr;
      Trace : in Boolean)
   is
      More_To_Check : Boolean := True;

      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LRk.Item_Lookahead_Ptr;

      Added_One     : Boolean;

      use type LRk.Item_Lookahead_Ptr;
   begin

      --  While there are new lookaheads we haven't propagated yet
      while More_To_Check loop

         --  Check every valid lookahead against every mapped item in every mapping
         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookahead_Set;
            while Lookahead /= null loop

               if Lookahead.Last > 0 then
                  To := Mapping.To;
                  while To /= null loop
                     LRk.Include
                       (Set   => To.Item.Lookahead_Set,
                        Value => Lookahead.all,
                        Added => Added_One);

                     if Trace and Added_One then
                        Ada.Text_IO.Put_Line ("Adding propagated lookahead:");
                        LRk.Print_Item (To.Item.all);
                        Ada.Text_IO.New_Line;
                        Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead));
                     end if;

                     More_To_Check := More_To_Check or Added_One;
                     To := To.Next;
                  end loop;
               end if;

               Lookahead := Lookahead.Next;
            end loop;

            Mapping := Mapping.Next;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --------------------------------------------------------------------------
   --  Calculate the LR(1) propogations from the given grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LR(1) kernels on output.
   --------------------------------------------------------------------------
   procedure Fill_In_Lookaheads
     (Grammar : in     Production_List.Instance;
      First   : in     LRk.Derivation_Matrix;
      Kernels : in out LRk.Item_Set_List;
      Trace   : in     Boolean)
   is

      Kernel           : LRk.Item_Set_Ptr := Kernels.Head;
      Kernel_Item      : LRk.Item_Ptr;
      Closure_Item     : LRk.Item_Ptr;

      Kernel_Item_Set  : LRk.Item_Set :=
        (Set       => new LRk.Item_Node,
         Goto_List => null,
         Index     => 0,
         Next      => null
         );

      Propagate_Lookahead : constant LRk.Item_Lookahead_Ptr :=
        new LRk.Item_Lookahead'(Last       => 0,
                                Lookaheads => (others => Tokenizer.Terminal_ID'First),
                                Next       => null
                                );

      Closure     : LRk.Item_Set;

      --  A list of item lookahead propagations
      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookahead_Set := Propagate_Lookahead;

      --  Go through all the kernel sets
      while Kernel /= null loop

         --  Go through every item in the kernel set
         Kernel_Item     := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod    := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Pointer := Kernel_Item.Pointer;

            Closure := LRk.Closure (Set     => Kernel_Item_Set,
                                    First   => First,
                                    Grammar => Grammar
                                    );

            --  Go through every item in that item's closure
            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Source_Item  => Kernel_Item,
                  Source_Set   => Kernel.all,
                  Closure_Item => Closure_Item.all,
                  Grammar      => Grammar,
                  Kernels      => Kernels,
                  Propagations => Propagation_List,
                  Trace        => Trace);

               Closure_Item := Closure_Item.Next;
            end loop;

            LRk.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Print_Propagations (Propagation_List);
      end if;

      --  Propagate the propagated lookaheads across the kernels
      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LRk.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   ----------------------------------------------------------------------------
   --  A trimmed Image.
   ----------------------------------------------------------------------------
   function Integer_Image (Subject : in Integer) return String is
      State_Image : String (1 .. 5);
   begin
      Ada.Integer_Text_IO.Put
        (To   => State_Image,
         Item => Subject);

      return Ada.Strings.Fixed.Trim (Source => State_Image, Side => Ada.Strings.Both);
   end Integer_Image;

   --------------------------------------------------------------------------
   --  Print the contents of the given parse action. This routine is
   --  provided for debugging purposes.
   --------------------------------------------------------------------------
   function Print_Parse_Action (Action  : in Parse_Action;
                                Kernels : in LRk.Item_Set_List) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Dest_Kernel : LRk.Item_Set_Ptr := Kernels.Head;
   begin
      case Action.Verb is
         when Shift =>
            Result := Result & "shift and goto state" & Integer_Image (Integer (Action.State)) &
              ":";
            while State_Index (Dest_Kernel.Index) /= Action.State loop
               Dest_Kernel := Dest_Kernel.Next;
            end loop;
            Result := Result & LRk.Image (Dest_Kernel.all);

         when Reduce =>
            Result := Result & "reduce the last" & Integer_Image (Action.Length) &
              " tokens using production " &
              LRk.Print_Item ((Prod          => Action.Production,
                               Pointer       => Token_List.Null_Iterator,
                               Lookahead_Set => null,
                               Next          => null
                               ));
         when Accept_It =>
            Result := Result & "accept it";
         when Error =>
            Result := Result & "ERROR";
      end case;

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Parse_Action;

   --------------------------------------------------------------------------
   --  Print the given Action node. This routine is included for
   --  debugging purposes.
   --------------------------------------------------------------------------
   function Print_Parse_Action (Action : in Parse_Action) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      case Action.Verb is
         when Shift =>
            Result := Result & "shift and goto state " & Integer_Image (Integer (Action.State));

         when Reduce =>
            Result := Result & "reduce the last " & Integer_Image (Action.Length) &
              " tokens using production" & Line_End & "   " &
              LRk.Print_Item ((Prod          => Action.Production,
                               Pointer       => Token_List.Null_Iterator,
                               Lookahead_Set => null,
                               Next          => null
                               ));
         when Accept_It =>
            Result := Result & "accept it";
         when Error =>
            Result := Result & "ERROR";
      end case;

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Parse_Action;

   --------------------------------------------------------------------------
   --  Print the given Action node. This routine is included for
   --  debugging purposes.
   --------------------------------------------------------------------------
   function Print_Action_Node (Node : in Action_Node) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      Result := Result & Tokenizer.Terminal_ID'Image (Node.Symbol) & " => " &
        Print_Parse_Action (Node.Action);

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Action_Node;

   --------------------------------------------------------------------------
   --  Print the given Reduction node. This routine is included for
   --  debugging purposes.
   --------------------------------------------------------------------------
   function Print_Reduction_Node (Node : in Reduction_Node) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      Result := Result & "on " & Token.Token_ID'Image (Node.Symbol) &
        " goto state " & Integer_Image (Integer (Node.State));

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Reduction_Node;

   ----------------------------------------------------------------------------
   --  Print the given parse state. This routine is included for debugging
   --  purposes.
   ----------------------------------------------------------------------------
   procedure Print_Parse_State (State : in Parse_State)
   is
      use Ada.Text_IO;
      Action    : Action_Node_Ptr    := State.Action_List;
      Reduction : Reduction_Node_Ptr := State.Reduction_List;
   begin
      Put_Line ("Actions:");
      while Action /= null loop
         Put_Line (Print_Action_Node (Action.all));
         Action := Action.Next;
      end loop;

      Put_Line ("Reductions:");
      while Reduction /= null loop
         Put_Line (Print_Reduction_Node (Reduction.all));
         Reduction := Reduction.Next;
      end loop;
   end Print_Parse_State;

   --------------------------------------------------------------------------
   --  Print the given parse table to Ada.Text_IO.Current_Output. This
   --  routine is included for debugging purposes.
   --------------------------------------------------------------------------
   procedure Print_Parse_Table (Table : in Parse_Table)
   is
      use Ada.Text_IO;
   begin
      for State in Table'Range loop
         Put_Line ("State " & Integer_Image (Integer (State)) & ":");
         Print_Parse_State (Table (State));
         New_Line;
      end loop;
   end Print_Parse_Table;

   ----------------------------------------------------------------------------
   --  Add a parse action to the given list of parse actions
   ----------------------------------------------------------------------------
   procedure Add_Action
     (Symbol      : in     Tokenizer.Terminal_ID;
      Action      : in     Parse_Action;
      Action_List : in out Action_Node_Ptr;
      --  These last parameters are for error reporting
      Source      : in     LRk.Item_Set;
      Kernels     : in     LRk.Item_Set_List;
      Conflicts   : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Matching_Action /= null then
         if Matching_Action.Action = Action then
            --  Matching_Action is identical to Action, so there is no
            --  conflict; just don't add it again.
            return;
         else
            --  There is a conflict
            Conflicts := Conflicts & Parse_Action_Verbs'Image (Matching_Action.Action.Verb) &
              "/" & Parse_Action_Verbs'Image (Action.Verb) & " in state:" & Line_End &
              LRk.Image (Source)
              & " on token " & Tokenizer.Terminal_ID'Image (Symbol) & Line_End &
              Print_Parse_Action
              (Action  => Matching_Action.Action,
               Kernels => Kernels) & Line_End & "   and" &
              Line_End &
              Print_Parse_Action
              (Action  => Action,
               Kernels => Kernels) & Line_End;
            return;
         end if;
      end if;

      Action_List := new Action_Node'
        (Symbol => Symbol,
         Action => Action,
         Next   => Action_List
         );
   end Add_Action;

   ----------------------------------------------------------------------------
   --  Fill in the parse table using the given LR(k) kernel sets.
   ----------------------------------------------------------------------------
   procedure Fill_In_Parse_Table
     (LRk_Kernels : in     LRk.Item_Set_List;
      Grammar     : in     Production_List.Instance;
      First       : in     LRk.Derivation_Matrix;
      Table       : in out Parse_Table;
      Trace       : in     Boolean)
   is
      use Ada.Strings.Unbounded;

      --  The default action, when nothing else matches an input
      Default_Action : constant Action_Node :=
        (Symbol => Tokenizer.Terminal_ID'First,
         Action => (Verb => Error),
         Next   => null);

      Last_Action : Action_Node_Ptr;

      Kernel    : LRk.Item_Set_Ptr := LRk_Kernels.Head;
      Closure   : LRk.Item_Set;
      Item      : LRk.Item_Ptr;
      Lookahead : LRk.Item_Lookahead_Ptr;

      Production_Length : Natural;
      RHS_Iterator      : Token_List.List_Iterator;

      Goto_Node : LRk.Set_Reference_Ptr;

      Conflicts : Unbounded_String := Null_Unbounded_String;

      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;
      use type LRk.Set_Reference_Ptr;
      use type LRk.Item_Lookahead_Ptr;
      use type Token_List.List_Iterator;
      use type Token.Handle;
   begin
      while Kernel /= null loop

         if Trace then
            Ada.Text_IO.Put_Line ("adding actions for kernel" & Integer'Image (Kernel.Index));
         end if;

         Closure := LRk.Closure
           (Set     => Kernel.all,
            First   => First,
            Grammar => Grammar);

         Item := Closure.Set;
         while Item /= null loop
            if Item.Pointer = Token_List.Null_Iterator then

               --  Find the length of the producion to save time during reductions
               Production_Length := 0;
               RHS_Iterator := Token_List.Initial_Iterator (Item.Prod.RHS.Tokens);
               while Token_List.Token_Handle (RHS_Iterator) /= null loop
                  Production_Length := Production_Length + 1;
                  Token_List.Next_Token (RHS_Iterator);
               end loop;

               if Trace then
                  Ada.Text_IO.Put_Line ("processing lookaheads");
               end if;

               Lookahead := Item.Lookahead_Set;
               while Lookahead /= null loop
                  --  Add reduction/accept action
                  if Production_List.Get_Production (Production_List.Initial_Iterator (Grammar)) = Item.Prod then
                     if Trace then
                        Ada.Text_IO.Put_Line ("adding Accept_It");
                     end if;

                     Add_Action
                       (Symbol        => Lookahead.Lookaheads (1),
                        Action        =>
                          (Verb       => Accept_It,
                           Production => Item.Prod,
                           Length     => Production_Length),
                        Action_List   => Table (State_Index (Kernel.Index)).Action_List,
                        Source        => Kernel.all,
                        Kernels       => LRk_Kernels,
                        Conflicts     => Conflicts);
                  else
                     if Trace then
                        Ada.Text_IO.Put_Line ("adding Reduce");
                     end if;

                     Add_Action
                       (Symbol        => Lookahead.Lookaheads (1),
                        Action        =>
                          (Verb       => Reduce,
                           Production => Item.Prod,
                           Length     => Production_Length),
                        Action_List   => Table (State_Index (Kernel.Index)).Action_List,
                        Source        => Kernel.all,
                        Kernels       => LRk_Kernels,
                        Conflicts     => Conflicts);
                  end if;

                  Lookahead := Lookahead.Next;
               end loop;
            elsif
              Token.ID (Token_List.Token_Handle (Item.Pointer).all) in
              Tokenizer.Terminal_ID
            then
               if Trace then
                  Ada.Text_IO.Put_Line
                    (Token.Token_ID'Image (Token.ID (Token_List.Token_Handle (Item.Pointer).all)) &
                       " => Shift");
               end if;

               Add_Action
                 (Symbol          => Token.ID (Token_List.Token_Handle (Item.Pointer).all),
                  Action          =>
                    (Verb         => Shift,
                     State        => State_Index
                       (LRk.Goto_Set
                          (From   => Kernel.all,
                           Symbol => Token.ID (Token_List.Token_Handle (Item.Pointer).all)).Index)),
                  Action_List     => Table (State_Index (Kernel.Index)).Action_List,
                  Source          => Kernel.all,
                  Kernels         => LRk_Kernels,
                  Conflicts       => Conflicts);

            else
               if Trace then
                  Ada.Text_IO.Put_Line
                    (Token.Token_ID'Image (Token.ID (Token_List.Token_Handle (Item.Pointer).all)) &
                       " => no action");
               end if;
            end if;

            Item := Item.Next;
         end loop;

         LRk.Free (Closure);

         if Length (Conflicts) /= 0 then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, To_String (Conflicts));
         end if;

         --  Fill in this item's Goto transitions
         Goto_Node := Kernel.Goto_List;
         while Goto_Node /= null loop
            Table (State_Index (Kernel.Index)).Reduction_List :=
              new Reduction_Node'
              (Symbol => Goto_Node.Symbol,
               State  => State_Index (Goto_Node.Set.Index),
               Next   => Table (State_Index (Kernel.Index)).Reduction_List);

            Goto_Node := Goto_Node.Next;
         end loop;

         --  Place a default error action at the end of every state.
         --  (it should always have at least one action already).
         Last_Action := Table (State_Index (Kernel.Index)).Action_List;

         if Last_Action = null then
            Table (State_Index (Kernel.Index)).Action_List := new Action_Node'(Default_Action);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Generating parser: state" & Integer'Image (Kernel.Index) & " has no actions");
         else
            while Last_Action.Next /= null loop
               Last_Action := Last_Action.Next;
            end loop;
            Last_Action.Next := new Action_Node'(Default_Action);
         end if;

         Kernel := Kernel.Next;
      end loop;

   end Fill_In_Parse_Table;

   ----------------------------------------------------------------------------
   --  Perform the specified reduction on the given token stack.
   ----------------------------------------------------------------------------
   procedure Reduce_Stack
     (Stack            : in out State_Node_Ptr;
      Number_Of_Tokens : in     Natural;
      Production       : in     OpenToken.Production.Instance
     ) is

      Arguments    : Token_List.Instance;
      Popped_State : State_Node_Ptr;

      Args_Added  : Natural := 0;

      use type Nonterminal.Synthesize;
   begin
      --  Pop the indicated number of token states from the stack, and
      --  send them to the synthesize routine (if any) to create a new
      --  nonterminal token.

      --  Build the argument list, while popping all but the last
      --  argument's state off of the stack.
      loop
         Token_List.Enqueue (List  => Arguments,
                             Token => Stack.Seen_Token
                             );

         Args_Added := Args_Added + 1;
         exit when Args_Added = Number_Of_Tokens;

         Popped_State := Stack;
         Stack := Stack.Next;
         Free (Popped_State);
      end loop;

      Production.RHS.Action (New_Token => Production.LHS.all,
                             Source    => Arguments,
                             To_ID     => Token.ID (Production.LHS.all)
                             );
      Token_List.Clean (Arguments);

      Stack.Seen_Token := new Nonterminal.Class'(Production.LHS.all);
   end Reduce_Stack;

   ----------------------------------------------------------------------------
   --  Create a new parser from the given grammar with the given token analyzer.
   ----------------------------------------------------------------------------
   overriding function Generate
     (Grammar  : in Production_List.Instance;
      Analyzer : in Tokenizer.Instance;
      Trace    : in Boolean                  := False)
     return Instance
   is
      New_Parser  : Instance;

      First_Tokens : constant LRk.Derivation_Matrix := LRk.First_Derivations (Grammar, Trace);

      Kernels : LRk.Item_Set_List := LRk.LR0_Kernels (Grammar, First_Tokens, Trace);

   begin
      New_Parser.Analyzer := Analyzer;

      Fill_In_Lookaheads
        (Grammar => Grammar,
         First   => First_Tokens,
         Kernels => Kernels,
         Trace   => Trace);

      if Trace then
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LRk.Print_Item_Set_List (Kernels);
         Ada.Text_IO.New_Line;
      end if;

      New_Parser.Table := new Parse_Table (1 .. State_Index (Kernels.Size));

      --  Add actions
      Fill_In_Parse_Table
        (LRk_Kernels => Kernels,
         Grammar     => Grammar,
         First       => First_Tokens,
         Table       => New_Parser.Table.all,
         Trace       => Trace);

      LRk.Free (Kernels);
      return New_Parser;
   end Generate;

   ----------------------------------------------------------------------------
   --  Attempt a parse. This routine will return when all available tokens
   --  have been parsed. (or an exception is raised)
   ----------------------------------------------------------------------------
   overriding procedure Parse (Parser : in out Instance)
   is
      Stack         : State_Node_Ptr;
      Current_State : State_Node;

      Action : Parse_Action;
      Popped_State : State_Node_Ptr;

      use type Token_List.Instance;
   begin

      --  Get the first token from the analyzer
      begin
         Tokenizer.Find_Next (Parser.Analyzer);
      exception
      when E : Syntax_Error =>
         raise Syntax_Error with
           Integer_Image (Line (Parser)) &
           ":" &
           Integer_Image (Column (Parser) - 1) &
           " " &
           Ada.Exceptions.Exception_Message (E);
      end;

      Current_State.Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

      Current_State.State := 1;
      loop

         --  Find the action for this token's ID
         Action := Action_For
           (Table => Parser.Table,
            State => Current_State.State,
            ID    => Token.ID (Current_State.Seen_Token.all)
            );

         if Parser.Trace then
            Ada.Text_IO.Put
              (Ada.Text_IO.Standard_Error,
               State_Index'Image (Current_State.State) &
                 " : " & Token.Token_ID_Type'Image (Token.ID (Current_State.Seen_Token.all)) &
                 " : " & Parse_Action_Verbs'Image (Action.Verb));
         end if;

         case Action.Verb is
            when Shift =>
               --  Push this token state on the stack
               Current_State.Next := Stack;
               Stack := new State_Node'(Current_State);

               --  Get the next token
               begin
                  Tokenizer.Find_Next (Parser.Analyzer);
               exception
               when E : Syntax_Error =>
                  raise Syntax_Error with
                    Integer_Image (Line (Parser)) &
                    ":" &
                    Integer_Image (Column (Parser) - 1) &
                    " " &
                    Ada.Exceptions.Exception_Message (E);
               end;

               Current_State.Seen_Token := new Token.Class'
                 (Token.Class (Tokenizer.Get (Parser.Analyzer)));

               Current_State.State := Action.State;

               if Parser.Trace then
                  Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
               end if;

            when Reduce =>

               --  Reduce by the indicated production
               Reduce_Stack
                 (Stack            => Stack,
                  Number_Of_Tokens => Action.Length,
                  Production       => Action.Production
                  );

               --  The next state is the one that the reduced state's goto for the
               --  LHS token takes us to.
               Current_State.State :=
                 Goto_For (Table => Parser.Table,
                           State => Stack.State,
                           ID    => Token.ID (Action.Production.LHS.all));

               if Parser.Trace then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     " to state" &
                       State_Index'Image (Current_State.State) &
                       " : " & Token.Token_ID_Type'Image (Token.ID (Action.Production.LHS.all)));
               end if;

            when Accept_It =>
               --  Reduce by the indicated production
               Reduce_Stack
                 (Stack            => Stack,
                  Number_Of_Tokens => Action.Length,
                  Production       => Action.Production
                  );

               if Parser.Trace then
                  Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
               end if;

               --  Clean up
               Free (Current_State.Seen_Token);
               while Stack /= null loop
                  Popped_State := Stack;
                  Stack := Stack.Next;
                  Free (Popped_State.Seen_Token);
                  Free (Popped_State);
               end loop;

               return;

            when Error =>
               if Parser.Trace then
                  Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
               end if;

               --  Clean up
               declare
                  ID     : constant String := Token.Token_ID'Image (Token.ID (Current_State.Seen_Token.all));
                  Lexeme : constant String := Tokenizer.Lexeme (Parser.Analyzer);
               begin
                  Free (Current_State.Seen_Token);
                  while Stack /= null loop
                     Popped_State := Stack;
                     Stack := Stack.Next;
                     Free (Popped_State.Seen_Token);
                     Free (Popped_State);
                  end loop;

                  raise Syntax_Error with
                    Integer_Image (Line (Parser)) &
                    ":" &
                    Integer_Image (Column (Parser) - 1) &
                    ": Syntax error; Unexpected " &
                    ID &
                    "'" &
                    Lexeme &
                    "'";
               end;
         end case;

      end loop;
   end Parse;

   ----------------------------------------------------------------------------
   --  This routine displays the parse table for the parser to
   --  Ada.Text_IO.Current_Output. This may be useful for debugging grammars
   --  (or, heaven forbid, the parser itself).
   ----------------------------------------------------------------------------
   procedure Print_Table (Parser : in Instance) is
   begin
      Ada.Text_IO.Put_Line ("Parse Table:");
      Print_Parse_Table (Parser.Table.all);
   end Print_Table;

   procedure Set_Trace (Parser : in out Instance; Enabled : in Boolean)
   is begin
      Parser.Trace := Enabled;
   end Set_Trace;

end OpenToken.Production.Parser.LALR;
