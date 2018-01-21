-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2009, 2010, 2013, 2014 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
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
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package is the root package of an implementation of a LALR
--  (Look-Ahead Left-to-right scanning Rightmost-deriving) parser for
--  grammars defined by a production list. It contains types shared by
--  the parse table generator in child package generator, and the
--  parser in child package parser. Debug output of structures is in
--  child package Text_IO.

with Ada.Containers.Doubly_Linked_Lists;
generic
   First_State_Index : in Natural;
package OpenToken.Production.Parser.LALR is

   --  No private types; that would make it too hard to write the unit tests

   --  Following are the types used in the parse table. The parse
   --  table is an array indexed by parse state that where each state
   --  contains a list of parse actions and a list of gotos.
   --
   --  Parse actions are indexed by the terminal they match and are either
   --    o Shift and change to a designated state.
   --    o Reduce by the given production
   --
   --  Gotos are indexed by the nonterminal they match and designate
   --  the state the parser need to change to.

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range Unknown_State_Index (First_State_Index) .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   type Parse_Action_Verbs is (Shift, Reduce, Accept_It, Error);
   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         State : State_Index;
      when Reduce | Accept_It =>
         LHS         : Nonterminal.Handle;
         Action      : Nonterminal.Synthesize;
         Index       : Integer; -- into rule, for generating action names, debugging.
         Token_Count : Natural;
      when Error =>
         null;
      end case;
   end record;
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   Null_Reduce_Action_Rec : constant Reduce_Action_Rec := (Reduce, null, null, 0, 0);

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;

   type Action_Node;
   type Action_Node_Ptr is access Action_Node;

   type Action_Node is record
      Symbol : Token.Terminal_ID;
      Action : Parse_Action_Node_Ptr;
      Next   : Action_Node_Ptr;
   end record;

   type Goto_Node;
   type Goto_Node_Ptr is access Goto_Node;

   type Goto_Node is record
      Symbol : Nonterminal_ID;
      State  : State_Index;
      Next   : Goto_Node_Ptr;
   end record;

   type Parse_State is record
      Action_List : Action_Node_Ptr;
      Goto_List   : Goto_Node_Ptr;
   end record;

   type Parse_Table is array (State_Index range <>) of Parse_State;

   type Parse_Table_Ptr is access Parse_Table;

   subtype Conflict_Parse_Actions is Parse_Action_Verbs range Shift .. Reduce;
   type Conflict is record
      --  A typical conflict is:
      --
      --  SHIFT/REDUCE in state: 11 on token IS
      --
      --  State numbers change with minor changes in the grammar, so
      --  we identify the state by the LHS of the two productions
      --  involved. We also store the state number for generated
      --  conflicts (not for known conflicts from the grammar
      --  definition file), for Text_IO output.
      Action_A    : Conflict_Parse_Actions;
      LHS_A       : Token.Token_ID;
      Action_B    : Conflict_Parse_Actions;
      LHS_B       : Token.Token_ID;
      State_Index : Unknown_State_Index;
      On          : Token.Token_ID;
   end record;

   package Conflict_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict);

   ----------
   --  Useful text output

   function State_Image (Item : in State_Index) return String;
   --  no leading space

   procedure Put (Item : in Parse_Action_Rec);

end OpenToken.Production.Parser.LALR;
