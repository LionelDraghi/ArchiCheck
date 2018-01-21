-------------------------------------------------------------------------------
--
-- Copyright (C) 2003, 2008, 2013, 2014 Stephe Leake
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

---------------------------------------------------------------------------
--  This package provides types and operatorion for parsing analysis
--  on grammars and LR(k) items.
---------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with OpenToken.Production.List;
generic
   type Unknown_State_Index is range <>;
   Unknown_State : in Unknown_State_Index;

   --  The number of elements of lookahead to keep in an item
   --  We only instantiate this with K = 1, but it's not worth
   --  optimizing the code; there is only one loop on 1 .. K.
   K : in Natural;

   with package Production_List is new OpenToken.Production.List;
package OpenToken.Production.Parser.LRk_Item is

   --  Lookahead Sets
   type Full_Lookahead_Set is array (1 .. K) of Token.Terminal_ID;
   type Item_Lookahead;
   type Item_Lookahead_Ptr is access Item_Lookahead;
   type Item_Lookahead is record
      Last       : Natural;
      Lookaheads : Full_Lookahead_Set;
      Next       : Item_Lookahead_Ptr;
   end record;

   --  Add Value to Set if it is not already present
   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead);

   --  Add Value to Set if it is not already present. Added will be
   --  true if the item had to be added.
   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead;
      Added :    out Boolean);

   --  The structure for actual items
   type Item_Node;
   type Item_Ptr is access Item_Node;
   type Item_Node is record
      Prod       : OpenToken.Production.Instance;
      Dot        : Token_List.List_Iterator; -- token after item Dot
      State      : Unknown_State_Index;
      Lookaheads : Item_Lookahead_Ptr;
      Next       : Item_Ptr;
   end record;

   --  Return an item node made from the given production, with Dot at
   --  the start of the right hand side. Lookaheads are copied.
   function Item_Node_Of
     (Prod       : in Production_List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Item_Lookahead_Ptr := null)
     return Item_Node;

   --  Return an item node made from the given production, with Dot at
   --  the start of the right hand side.
   function Item_Node_Of
     (Prod  : in OpenToken.Production.Instance;
      State : in Unknown_State_Index)
     return Item_Node;

   type Set_Reference;
   type Set_Reference_Ptr is access Set_Reference;

   type Item_Set;
   type Item_Set_Ptr is access Item_Set;

   type Set_Reference is record
      Set    : Item_Set_Ptr;
      Symbol : Token.Token_ID;
      Next   : Set_Reference_Ptr;
   end record;

   type Item_Set is record
      Set       : Item_Ptr;
      Goto_List : Set_Reference_Ptr;
      State     : Unknown_State_Index;
      Next      : Item_Set_Ptr;
   end record;

   type Item_Set_List is record
      Head : Item_Set_Ptr;
      Size : Unknown_State_Index := 0;
   end record;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set);
   --  Add an item to the set without checking to see if it is in there already.

   function Find
     (Left  : in Item_Node;
      Right : in Item_Set)
     return Item_Ptr;
   --  Return a pointer to an item in Right that matches Left.Prod,
   --  Left.Dot, null if not found.

   function Find
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to Left in Right, null if not found.

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to the set in Sets containing State, null if not found.

   function Is_In
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Boolean;

   function Is_In
     (Set_Ptr   : in Item_Set_Ptr;
      Symbol    : in Token.Token_ID;
      Goto_List : in Set_Reference_Ptr)
     return Boolean;

   function Goto_Set
     (From   : in Item_Set;
      Symbol : in Token.Token_ID)
     return Item_Set_Ptr;

   ------------------------------------------------
   --  Types and operations for computing Item sets

   type Token_ID_Set is array (Token.Token_ID) of Boolean;

   function Image (Item : in Token_ID_Set) return String;

   type Derivation_Matrix is array (Nonterminal_ID) of Token_ID_Set;

   type Nonterminal_ID_Set is array (Nonterminal_ID) of Boolean;

   function Has_Empty_Production (Grammar : in Production_List.Instance) return Nonterminal_ID_Set;

   function First_Derivations
     (Grammar              : in Production_List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Trace                : in Boolean)
     return Derivation_Matrix;
   --  For each nonterminal in the given grammar, find the set of
   --  tokens that its first term could start with.

   function Lookahead_Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production_List.Instance;
      Trace                : in Boolean)
     return Item_Set;
   --  Return the lookahead closure of Set over Grammar. First must be
   --  the result of First_Derivations.

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Item_Lookahead, Item_Lookahead_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Item_Set, Item_Set_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Set_Reference, Set_Reference_Ptr);

   procedure Free (Subject : in out Item_Node);
   procedure Free (Subject : in out Item_Set);
   procedure Free (Subject : in out Item_Set_List);

   function LR0_Kernels
     (Grammar           : in Production_List.Instance;
      First             : in Derivation_Matrix;
      Trace             : in Boolean;
      First_State_Index : in Unknown_State_Index)
     return Item_Set_List;

   function Print (Item : in Item_Lookahead) return String;
   function Print (Item : in Item_Lookahead_Ptr) return String;

   procedure Put (Item : in Item_Node; Show_Lookaheads : in Boolean);
   procedure Put (Item : in Item_Set);
   procedure Put (Item : in Set_Reference_Ptr);
   procedure Put (Item : in Item_Set_List);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

   ----------
   --  visible for unit test

   function Goto_Transitions
     (Kernel  : in Item_Set;
      Symbol  : in Token.Token_ID;
      First   : in Derivation_Matrix;
      Grammar : in Production_List.Instance)
     return Item_Set;

end OpenToken.Production.Parser.LRk_Item;
