-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: opentoken-production-parser-lalr.adb,v $
-- Revision 1.3  2000/08/12 23:58:06  Ted
-- Removed unused variables
--
-- Revision 1.2  2000/08/06 23:42:58  Ted
-- Fix token parsing to be dynamicly dispatching
--
-- Revision 1.1  2000/01/27 20:51:42  Ted
-- An LALR parser implementation
--
--
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Integer_Text_IO;

with OpenToken.Production.Parser.LRK_Item;

use type Ada.Strings.Unbounded.Unbounded_String;
-------------------------------------------------------------------------------
-- This package provides an implementation of a LALR (Look-Ahead Left-to-right
-- scanning Rightmost-deriving) parser for grammars defined by a production
-- list. This is probably the most popular method due to it being a good
-- trade-off between the amount of grammars handled and the size of its parse
-- table.
-------------------------------------------------------------------------------
package body OpenToken.Production.Parser.LALR is

   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   package LRk is new OpenToken.Production.Parser.LRK_Item (1);

   ----------------------------------------------------------------------------
   -- Following are the types used in the parse "table". The parse table is
   -- an array indexed by parse state that where each state contains a list of
   -- parse actions and a list of reduction actions.
   --
   -- Parse actions are indexed by the terminal they match and are either
   --    o Shift and change to a designated state.
   --    o Reduce by the given production
   --
   -- Reduction actions are indexd by the nonterminal they match and designate
   -- the state the parser need to change to.

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

   ----------------------------------------------------------------------------
   -- The following types are used for the Parser's stack. The stack
   -- designates the tokens that have been read or derived, and the parser
   -- states in which that occurred.


   type State_Node;
   type State_Node_Ptr is access State_Node;

   type State_Node is record
      State      : State_Index := 0;
      Seen_Token : Token.Handle;
      Next       : State_Node_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (State_Node, State_Node_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Token.Class, Token.Handle);


   ----------------------------------------------------------------------------
   -- The following types are used for computing lookahead propagations

   type Item_List;
   type Item_List_Ptr is access Item_List;
   type Item_List is record
      Item : LRK.Item_Ptr;
      Next : Item_List_Ptr;
   end record;

   type Item_Item_List_Mapping;
   type Item_Item_List_Mapping_Ptr is access Item_Item_List_Mapping;

   type Item_Item_List_Mapping is record
      From : LRK.Item_Ptr;
      To   : Item_List_Ptr;
      Next : Item_Item_List_Mapping_Ptr;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);


   ----------------------------------------------------------------------------
   -- Free the storage allocated by this package for the given item list.
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

   ----------------------------------------------------------------------------
   -- Free the storage allocated by this package for the given item item list
   -- mapping list.
   ----------------------------------------------------------------------------
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
   -- Return the action for the given state index and terminal ID.
   -- The final node for a state is assumed to match all inputs.
   ----------------------------------------------------------------------------
   function Action_For (Table : in Parse_Table_Ptr;
                        State : in State_Index;
                        ID    : in Tokenizer.Terminal_ID
                       ) return Parse_Action is

      use type Tokenizer.Terminal_ID;
      Action_Node : Action_Node_Ptr := Table.all(State).Action_List;
   begin
      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      return Action_Node.Action;
   end Action_For;

   ----------------------------------------------------------------------------
   -- Return the action for the given state index and terminal ID.
   -- The final node for a state is assumed to match all inputs.
   ----------------------------------------------------------------------------
   function Goto_For (Table : in Parse_Table_Ptr;
                      State : in State_Index;
                      ID    : in Token.Token_ID
                     ) return State_Index is

      use type Tokenizer.Terminal_ID;
      Reduction_Node : Reduction_Node_Ptr := Table.all(State).Reduction_List;
   begin
      while Reduction_Node.Next /= null and Reduction_Node.Symbol /= ID loop
         Reduction_Node := Reduction_Node.Next;
      end loop;

      return Reduction_Node.State;
   end Goto_For;

   ----------------------------------------------------------------------------
   -- Locate the action node for the non-terminal ID in the given table.
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
   -- Display the given propagations. This routine is included mainly as a
   -- debugging aid.
   ----------------------------------------------------------------------------
   procedure Print_Propagations (Propagations : Item_Item_List_Mapping_Ptr) is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      Ada.Text_IO.Put_Line ("Propagations:");

      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LRK.Print_Item (Next_Prop.From.all);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("          To ");
            LRK.Print_Item (Next_To.Item.all);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Print_Propagations;

   ----------------------------------------------------------------------------
   -- Add propagation entires (if they don't already exist) from the given item
   -- to all kernel items that match the given To item.
   ----------------------------------------------------------------------------
   procedure Add_Propagations
     (From         : in     LRK.Item_Ptr;
      From_Set     : in     LRK.Item_Set;
      To           : in     LRK.Item_Node;
      For_Token    : in     Token.Token_ID;
      Kernels      : in     LRK.Item_Set_List;
      Propagations : in out Item_Item_List_Mapping_Ptr
     ) is

      Kernel_Set : LRK.Item_Set_Ptr := Kernels.Head;
      To_Kernel  : LRK.Item_Ptr;

      Prop_Match    : Item_Item_List_Mapping_Ptr;
      Prop_To_Match : Item_List_Ptr;
      Found_From    : Boolean;
      Found_To      : Boolean;

      use type LRK.Item_Set_Ptr;
      use type LRK.Item_Ptr;
   begin

      -- For the kernel element that matches the given item in the goto for the
      -- given from set on the given token...
      To_Kernel := LRK.Find
        (Left  => To,
         Right => LRK.Goto_Set
         (From   => From_Set,
          Symbol => For_Token
          ).all
         );

      if To_Kernel /= null then

         -----------------------------------------------------------
         -- If there isn't already a lookahead mapping for that item
         -- and the source item, make one

         -- Look through all the propagations...
         Found_From := False;
         Prop_Match := Propagations;
     Find_Matching_Prop:
         while Prop_Match /= null loop
            if Prop_Match.From = From then

               -- Look through all the propagation mappings...
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

   ----------------------------------------------------------------------------
   -- For the given source and closure item, calculate the lookaheads. If it
   -- is a spontanious lookahead, put it in the source item's lookahead. If
   -- it is a propagated lookahead, put the appropriate entry on the
   -- propagation list.
   ----------------------------------------------------------------------------
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LRK.Item_Ptr;
      Source_Set   : in     LRK.Item_Set;
      Closure_Item : in     LRK.Item_Node;
      Grammar      : in     Production_List.Instance;
      Kernels      : in     LRK.Item_Set_List;
      Propagations : in out Item_Item_List_Mapping_Ptr
     ) is

      Next_Item   : LRK.Item_Node;
      Next_Token  : Token_List.List_Iterator;
      Next_Kernel : LRK.Item_Ptr;
      Lookahead   : LRK.Item_Lookahead_Ptr := Closure_Item.Lookahead_Set;

      use type Token.Handle;
      use type LRK.Item_Set_Ptr;
      use type LRK.Item_Ptr;
      use type LRK.Item_Lookahead_Ptr;
   begin

      -- Verify that the closure item has a token past the pointer
      if Token_List.Token_Handle (Closure_Item.Pointer) = null then
         return;
      end if;
      Next_Token := Closure_Item.Pointer;
      Token_List.Next_Token (Next_Token);
      Next_Item :=
        (Prod          => Closure_Item.Prod,
         Pointer       => Next_Token,
         Lookahead_Set => null,
         Next          => null
         );

      -- If this is the first production with the pointer on the last item, it
      -- gets a lookahead for each terminal. (so it will reduce on anything).
      if Source_Set.Index = 1 and Token_List.Token_Handle(Next_Token) = null then
         for Token_ID in Tokenizer.Terminal_ID loop
            LRK.Include
              (Set   => Source_Item.Lookahead_Set,
               Value => (Last       => 1,
                         Lookaheads => (1 => Token_ID),
                         Next       => null
                         )
               );
         end loop;
      end if;

      -- Check all of the closure item's lookaheads
      while Lookahead /= null loop
         if Lookahead.Last = 0 then
            -- Lookaheads propagate
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To           => Next_Item,
               For_Token    => Token.ID(Token_List.Token_Handle (Closure_Item.Pointer).all),
               Kernels      => Kernels,
               Propagations => Propagations
               );

         else
            -- Lookaheads are generated spontaniously for all items
            -- in the source item's goto for the current symbol that
            -- match the next_item.

            Next_Kernel := LRK.Find
              (Left  => Next_Item,
               Right => LRK.Goto_Set
               (From   => Source_Set,
                Symbol => Token.ID(Token_List.Token_Handle (Closure_Item.Pointer).all)
                ).all
               );

            if Next_Kernel /= null then
               LRK.Include
                 (Set   => Next_Kernel.Lookahead_Set,
                  Value => Lookahead.all
                  );
            end if;

         end if;

         Lookahead := Lookahead.Next;
      end loop;
   end Generate_Lookahead_Info;

   ----------------------------------------------------------------------------
   -- Propagate lookaheads as directed by the given propagation list, until no
   -- more lookaheads are propagated.
   ----------------------------------------------------------------------------
   procedure Propagate_Lookaheads (List : in Item_Item_List_Mapping_Ptr) is
      More_To_Check : Boolean := True;

      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LRK.Item_Lookahead_Ptr;

      Added_One     : Boolean;

      use type LRK.Item_Lookahead_Ptr;
   begin

      -- While there are new lookaheads we haven't propagated yet
      while More_To_Check loop

         -- Check every valid lookahead against every mapped item in every mapping
         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookahead_Set;
            while Lookahead /= null loop

               if Lookahead.Last > 0 then
                  To := Mapping.To;
                  while To /= null loop
                     LRK.Include
                       (Set   => To.Item.Lookahead_Set,
                        Value => Lookahead.all,
                        Added => Added_One
                        );

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

   ----------------------------------------------------------------------------
   -- Calculate the LR(1) propogations from the given grammar. Kernels should
   -- be the sets of LR(0) kernels on input, and will become the set of
   -- LR(1) kernels on output.
   ----------------------------------------------------------------------------
   procedure Fill_In_Lookaheads
     (Grammar : in     Production_List.Instance;
      First   : in     LRK.Derivation_Matrix;
      Kernels : in out LRK.Item_Set_List) is

      Kernel           : LRK.Item_Set_Ptr := Kernels.Head;
      Kernel_Item      : LRK.Item_Ptr;
      Closure_Item     : LRK.Item_Ptr;

      Kernel_Item_Set  : LRK.Item_Set :=
        (Set       => new LRK.Item_Node,
         Goto_List => null,
         Index     => 0,
         Next      => null
         );

      Propagate_Lookahead :  LRK.Item_Lookahead_Ptr :=
        new LRK.Item_Lookahead'(Last       => 0,
                                Lookaheads => (others => Tokenizer.Terminal_ID'First),
                                Next       => null
                                );

      Closure     : LRK.Item_Set;

      -- A list of item lookahead propagations
      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LRK.Item_Set_Ptr;
      use type LRK.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookahead_Set := Propagate_Lookahead;

      -- Go through all the kernel sets
      while Kernel /= null loop

         -- Go through every item in the kernel set
         Kernel_Item     := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod    := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Pointer := Kernel_Item.Pointer;

            Closure := LRK.Closure (Set     => Kernel_Item_Set,
                                    First   => First,
                                    Grammar => Grammar
                                    );

            -- Go through every item in that item's closure
            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Source_Item  => Kernel_Item,
                  Source_Set   => Kernel.all,
                  Closure_Item => Closure_Item.all,
                  Grammar      => Grammar,
                  Kernels      => Kernels,
                  Propagations => Propagation_List
                  );

               Closure_Item := Closure_Item.Next;
            end loop;

            LRK.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      -- Propagate the propagated lookaheads across the kernels
      Propagate_Lookaheads (Propagation_List);

      Free (Propagation_List);
      LRk.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   ----------------------------------------------------------------------------
   -- The following routines provide local access to various objects' image
   -- attributes. They are needed to work around a gnat bug.
   ----------------------------------------------------------------------------
   function Integer_Image (Subject : in Integer) return String is
      State_Image : String (1..5);
   begin
      Ada.Integer_Text_IO.Put
        (To   => State_Image,
         Item => Subject);

      return Ada.Strings.Fixed.Trim (Source => State_Image, Side => Ada.Strings.Both);
      --return Integer'Image(Integer(Subject));
   end Integer_Image;

   ----------------------------------------------------------------------------
   -- Print the contents of the given parse action. This routine is provided
   -- for debugging purposes.
   ----------------------------------------------------------------------------
   function Print_Parse_Action (Action  : in Parse_Action;
                                Kernels : in LRK.Item_Set_List) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Dest_Kernel : LRK.Item_Set_Ptr := Kernels.Head;
   begin
      case Action.Verb is
         when Shift =>
            Result := Result & "shift and goto state" & Integer_Image(Integer(Action.State)) &
              ":";
            while State_Index(Dest_Kernel.Index) /= Action.State loop
               Dest_Kernel := Dest_Kernel.Next;
            end loop;
            Result := Result & LRK.Print_Item_Set (Dest_Kernel.all);

         when Reduce =>
            Result := Result & "reduce the last" & Integer_Image(Action.Length) &
              " tokens using production " &
              LRK.Print_Item ((Prod          => Action.Production,
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

   ----------------------------------------------------------------------------
   -- Print the given Action node. This routine is included for debugging
   -- purposes.
   ----------------------------------------------------------------------------
   function Print_Parse_Action (Action : in Parse_Action) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      case Action.Verb is
         when Shift =>
            Result := Result & "shift and goto state " & Integer_Image(Integer(Action.State));

         when Reduce =>
            Result := Result & "reduce the last " & Integer_Image(Action.Length) &
              " tokens using production" & CRLF & "   " &
              LRK.Print_Item ((Prod          => Action.Production,
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

   ----------------------------------------------------------------------------
   -- Print the given Action node. This routine is included for debugging
   -- purposes.
   ----------------------------------------------------------------------------
   function Print_Action_Node (Node : in Action_Node) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      Result := Result & Tokenizer.Terminal_ID'Image(Node.Symbol) & " => " &
        Print_Parse_Action (Node.Action);

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Action_Node;

   ----------------------------------------------------------------------------
   -- Print the given Reduction node. This routine is included for debugging
   -- purposes.
   ----------------------------------------------------------------------------
   function Print_Reduction_Node (Node : in Reduction_Node) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      Result := Result & "on " & Token.Token_ID'Image(Node.Symbol) &
        " goto state " & Integer_Image(Integer(Node.State));

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Reduction_Node;

   ----------------------------------------------------------------------------
   -- Print the given parse state. This routine is included for debugging
   -- purposes.
   ----------------------------------------------------------------------------
   function Print_Parse_State (State : in Parse_State) return String is
      Action    : Action_Node_Ptr := State.Action_List;
      Reduction : Reduction_Node_Ptr := State.Reduction_List;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      while Action /= null loop
         Result := Result & Print_Action_Node (Action.all) & CRLF;

         Action := Action.Next;
      end loop;
      while Reduction /= null loop
         Result := Result & Print_Reduction_Node (Reduction.all) & CRLF;

         Reduction := Reduction.Next;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Parse_State;

   ----------------------------------------------------------------------------
   -- Print the given parse table. This routine is included for debugging
   -- purposes.
   ----------------------------------------------------------------------------
   function Print_Parse_Table (Table : in Parse_Table) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      for State in Table'Range loop
         Result := Result & "State " & Integer_Image(Integer(State)) & ":" &
           CRLF & Print_Parse_State (Table(State)) & CRLF;
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Parse_Table;

   ----------------------------------------------------------------------------
   -- Add a parse action to the given list of parse actions
   ----------------------------------------------------------------------------
   procedure Add_Action (Symbol      : in     Tokenizer.Terminal_ID;
                         Action      : in     Parse_Action;
                         Action_List : in out Action_Node_Ptr;
                         -- These last parameters are for error reporting
                         Source      : in     LRK.Item_Set;
                         Kernels     : in     LRK.Item_Set_List;
                         Conflicts   : in out Ada.Strings.Unbounded.Unbounded_String) is
      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Matching_Action /= null then
         -- If there is a conflict, add text about it to the list of conflict messages.
         Conflicts := Conflicts & Parse_Action_Verbs'Image(Matching_Action.Action.Verb) &
           "/" & Parse_Action_Verbs'Image(Action.Verb) & " in state:" & CRLF &
           LRk.Print_Item_Set (Source)
           & " on token " & Tokenizer.Terminal_ID'Image(Symbol) & CRLF &
           Print_Parse_Action
           (Action  => Matching_Action.Action,
            Kernels => Kernels) & CRLF & "   and" &
           CRLF &
           Print_Parse_Action
           (Action  => Action,
            Kernels => Kernels) & CRLF;
         return;
      end if;

      Action_List := new Action_Node'
        (Symbol => Symbol,
         Action => Action,
         Next   => Action_List
         );
   end Add_Action;

   ----------------------------------------------------------------------------
   -- Fill in the parse table using the given LR(k) kernel sets.
   ----------------------------------------------------------------------------
   procedure Fill_In_Parse_Table (LRk_Kernels : in     LRK.Item_Set_List;
                                  Grammar     : in     Production_List.Instance;
                                  First       : in     LRK.Derivation_Matrix;
                                  Table       : in out Parse_Table
                                 ) is

      -- The default action, when nothing else matches an input
      Default_Action : constant Action_Node :=
        (Symbol => Tokenizer.Terminal_ID'First,
         Action => (Verb => Error),
         Next   => null
         );
      Last_Action : Action_Node_Ptr;

      Kernel    : LRK.Item_Set_Ptr := LRk_Kernels.Head;
      Closure   : LRK.Item_Set;
      Item      : LRK.Item_Ptr;
      Lookahead : LRK.Item_Lookahead_Ptr;

      Production_Length : Natural;
      RHS_Iterator      : Token_List.List_Iterator;

      Goto_Node : LRK.Set_Reference_Ptr;

      Conflicts : Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.Null_Unbounded_String;


      type Index_Kernel_Map is array (Table'Range) of LRK.Item_Set_Ptr;

      use type LRK.Item_Ptr;
      use type LRK.Item_Set_Ptr;
      use type LRK.Set_Reference_Ptr;
      use type LRK.Item_Lookahead_Ptr;
      use type Token_List.List_Iterator;
      use type Token.Handle;
   begin
      while Kernel /= null loop

         Closure := LRK.Closure (Set     => Kernel.all,
                                 First   => First,
                                 Grammar => Grammar
                                 );

         Item := Closure.Set;
         while Item /= null loop
            if Item.Pointer = Token_List.Null_Iterator then

               -- Find the length of the producion to save time during reductions
               Production_Length := 0;
               RHS_Iterator := Token_List.Initial_Iterator(Item.Prod.RHS.Tokens);
               while Token_List.Token_Handle(RHS_Iterator) /= null loop
                  Production_Length := Production_Length + 1;
                  Token_List.Next_Token(RHS_Iterator);
               end loop;

               Lookahead := Item.Lookahead_Set;
               while Lookahead /= null loop
                  -- Add reduction/accept action
                  if Production_List.Get_Production(Production_List.Initial_Iterator (Grammar)) = Item.Prod then
                     Add_Action
                       (Symbol      => Lookahead.Lookaheads(1),
                        Action      => (Verb       => Accept_It,
                                        Production => Item.Prod,
                                        Length     => Production_Length
                                        ),
                        Action_List => Table(State_Index(Kernel.Index)).Action_List,
                        Source      => Kernel.all,
                        Kernels     => LRk_Kernels,
                        Conflicts   => Conflicts
                        );
                  else
                     Add_Action
                       (Symbol      => Lookahead.Lookaheads(1),
                        Action      => (Verb       => Reduce,
                                        Production => Item.Prod,
                                        Length     => Production_Length
                                        ),
                        Action_List => Table(State_Index(Kernel.Index)).Action_List,
                        Source      => Kernel.all,
                        Kernels     => LRk_Kernels,
                        Conflicts   => Conflicts
                        );
                  end if;

                  Lookahead := Lookahead.Next;
               end loop;
            elsif
              Token.ID(Token_List.Token_Handle(Item.Pointer).all) in
              Tokenizer.Terminal_ID
            then
               -- Add shift action
               Add_Action
                 (Symbol      => Token.ID(Token_List.Token_Handle(Item.Pointer).all),
                  Action      => (Verb  => Shift,
                                  State => State_Index
                                  (LRK.Goto_Set
                                   (From   => Kernel.all,
                                    Symbol => Token.ID(Token_List.Token_Handle(Item.Pointer).all)
                                    ).Index
                                   )
                                  ),
                  Action_List => Table(State_Index(Kernel.Index)).Action_List,
                  Source      => Kernel.all,
                  Kernels     => LRk_Kernels,
                  Conflicts   => Conflicts
                  );

            end if;

            Item := Item.Next;
         end loop;

         LRK.Free (Closure);

         -- Fill in this item's Goto transitions
         Goto_Node := Kernel.Goto_List;
         while Goto_Node /= null loop
            Table(State_Index(Kernel.Index)).Reduction_List :=
              new Reduction_Node'
              (Symbol => Goto_Node.Symbol,
               State  => State_Index(Goto_Node.Set.Index),
               Next   => Table(State_Index(Kernel.Index)).Reduction_List
               );

            Goto_Node := Goto_Node.Next;
         end loop;

         -- Place a default error action at the end of every state.
         -- (it should always have at least one action already).
         Last_Action := Table(State_Index(Kernel.Index)).Action_List;
         while Last_Action.Next /= null loop
            Last_Action := Last_Action.Next;
         end loop;
         Last_Action.Next := new Action_Node'(Default_Action);

         Kernel := Kernel.Next;
      end loop;

   end Fill_In_Parse_Table;

   ----------------------------------------------------------------------------
   -- Perform the specified reduction on the given token stack.
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
      -- Pop the indicated number of token states from the stack, and
      -- send them to the synthesize routine (if any) to create a new
      -- nonterminal token.

      -- Build the argument list, while popping all but the last argument's
      -- state off of the stack.
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
                             To_ID     => Token.ID(Production.LHS.all)
                             );
      Token_List.Clean (Arguments);

      Stack.Seen_Token := new Nonterminal.Class'(Production.LHS.all);
   end Reduce_Stack;

   ----------------------------------------------------------------------------
   -- Create a new parser from the given grammar with the given token analyzer.
   ----------------------------------------------------------------------------
   function Generate (Grammar  : in Production_List.Instance;
                      Analyzer : in Tokenizer.Instance) return Instance is
      New_Parser  : Instance;
      Iterator : Production_List.List_Iterator := Production_List.Initial_Iterator(Grammar);

      Kernels : LRK.Item_Set_List := LRK.LR0_Kernels (Grammar);

      First   : constant LRK.Derivation_Matrix := LRK.First_Derivations (Grammar);


      Set     : LRK.Item_Set_Ptr := Kernels.Head;

   begin
      New_Parser.Analyzer := Analyzer;

      -- Fill in the lookaheads for the grammar from its kernels
      Fill_In_Lookaheads
        (Grammar => Grammar,
         First   => First,
         Kernels => Kernels
         );

      -- Allocate a parse table with an entry for each set of kernels
      New_Parser.Table := new Parse_Table(1..State_Index(Kernels.Size));

      -- Fill in the parse table actions
      Fill_In_Parse_Table
        (LRk_Kernels => Kernels,
         Grammar     => Grammar,
         First       => First,
         Table       => New_Parser.Table.all
         );

      LRK.Free(Kernels);
      return New_Parser;
   end Generate;

   ----------------------------------------------------------------------------
   -- Attempt a parse. This routine will return when all available tokens
   -- have been parsed. (or an exception is raised)
   ----------------------------------------------------------------------------
   procedure Parse (Parser : in out Instance) is
      Stack         : State_Node_Ptr;
      Current_State : State_Node;

      Action : Parse_Action;
      Popped_State : State_Node_Ptr;

      use type Token_List.Instance;
   begin

      -- Get the first token from the analyzer
      Tokenizer.Find_Next (Parser.Analyzer);
      Current_State.Seen_Token := new Token.Class'(Token.Class(Tokenizer.Get (Parser.Analyzer)));

      Current_State.State := 1;
      loop
         -- Find the action for this token's ID
         Action := Action_For
           (Table => Parser.Table,
            State => Current_State.State,
            ID    => Token.ID(Current_State.Seen_Token.all)
            );

         case Action.Verb is
            when Shift =>
               -- Push this token state on the stack
               Current_State.Next := Stack;
               Stack := new State_Node'(Current_State);

               -- Get the next token
               Tokenizer.Find_Next (Parser.Analyzer);
               Current_State.Seen_Token := new Token.Class'
                 (Token.Class(Tokenizer.Get (Parser.Analyzer)));

               Current_State.State := Action.State;

            when Reduce =>

               -- Reduce by the indicated production
               Reduce_Stack
                 (Stack            => Stack,
                  Number_Of_Tokens => Action.Length,
                  Production       => Action.Production
                  );

               -- The next state is the one that the reduced state's goto for the
               -- LHS token takes us to.
               Current_State.State :=
                 Goto_For (Table => Parser.Table,
                           State => Stack.State,
                           ID    => Token.ID(Action.Production.LHS.all)
                           );

            when Accept_It =>
               -- Reduce by the indicated production
               Reduce_Stack
                 (Stack            => Stack,
                  Number_Of_Tokens => Action.Length,
                  Production       => Action.Production
                  );

               -- Clean up
               Free (Current_State.Seen_Token);
               while Stack /= null loop
                  Popped_State := Stack;
                  Stack := Stack.Next;
                  Free (Popped_State.Seen_Token);
                  Free (Popped_State);
               end loop;

               return;
            when Error =>
               -- Clean up
               declare
                  ID : constant String := Token.Token_ID'Image (Token.ID(Current_State.Seen_Token.all));
               begin
                  Free (Current_State.Seen_Token);
                  while Stack /= null loop
                     Popped_State := Stack;
                     Stack := Stack.Next;
                     Free (Popped_State.Seen_Token);
                     Free (Popped_State);
                  end loop;

                  Ada.Exceptions.Raise_Exception
                    (Parse_Error'Identity,
                     "at line " & Integer_Image(Tokenizer.Line (Parser.Analyzer)) &
                     " column " & Integer_Image(Tokenizer.Column (Parser.Analyzer)) & "." & CRLF &
                     "   Unexpected token " & ID & " (S" & Integer_Image(Integer(Current_State.State)) &
                     ").");
               end;
         end case;

      end loop;
   exception
      when Error : Syntax_Error =>
         Ada.Exceptions.Raise_Exception
           (Syntax_Error'Identity,
            "at line " & Integer_Image(Tokenizer.Line (Parser.Analyzer)) &
            " column " & Integer_Image(Tokenizer.Column (Parser.Analyzer)) & "." & CRLF &
            "   " & Ada.Exceptions.Exception_Message(Error));

   end Parse;

   ----------------------------------------------------------------------------
   -- Free any resources used by the given parser. It will be invalid after
   -- this call.
   ----------------------------------------------------------------------------
   procedure Cleanup (Parser : in out Instance) is
   begin
      null;
   end Cleanup;

   ----------------------------------------------------------------------------
   -- This routine displays the parse table for the parser to
   -- Ada.Text_IO.Current_Output. This may be useful for debugging grammars
   -- (or, heaven forbid, the parser itself).
   ----------------------------------------------------------------------------
   procedure Print_Table (Parser : in Instance) is
   begin
      Ada.Text_IO.Put_Line ("Parse Table:");
      Ada.Text_IO.Put_Line (Print_Parse_Table(Parser.Table.all));
   end Print_Table;

end OpenToken.Production.Parser.LALR;
