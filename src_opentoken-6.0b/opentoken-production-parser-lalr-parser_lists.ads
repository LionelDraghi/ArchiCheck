-------------------------------------------------------------------------------
--
-- Copyright (C) 2014 Stephe Leake
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
--  This package provides utilities used by the LALR parser. It is a
--  library package, rather than private within LALR.Parser, for unit
--  tests.

with Ada.Iterator_Interfaces;
generic
   First_Parser_Label : in Integer;
package OpenToken.Production.Parser.LALR.Parser_Lists is

   type Parser_State is private;

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Parser_State;

   function Initialize return List;

   function Count (List : in Parser_Lists.List) return Integer;

   type Cursor is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Integer;

   function Label (Cursor : in Parser_Lists.Cursor) return Integer;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;

   --  Parser stack
   type Stack_Item is record
      State : Unknown_State_Index;
      Token : OpenToken.Production.Token.Handle;
   end record;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Peek (Cursor : in Parser_Lists.Cursor) return Stack_Item;
   function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item;
   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item);

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean;

   procedure Put_Top_10 (Cursor : in Parser_Lists.Cursor);
   --  Put image of top 10 stack items to Current_Output.

   --  pending user actions
   type Action_Token is record
      Action    : Reduce_Action_Rec;
      New_Token : Nonterminal.Handle;
      Tokens    : Token_List.Instance;
   end record;

   Null_Action_Token : constant Action_Token := (Null_Reduce_Action_Rec, null, Token_List.Null_List);

   function Action_Tokens_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Action_Token_Count (Cursor : in Parser_Lists.Cursor) return Integer;
   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token);
   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
   --  Copy parser at Cursor, add to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class);
   --  Move Cursor to the internal free list, free its stack and
   --  pending actions; it will not appear in future iterations. On
   --  return, Cursor points to next parser (or none).

   ----------
   --  Stuff for iterators, to allow
   --  'for Parser of Parsers loop'
   --  'for Cursor in Parsers.First loop'
   --
   --  requires Parser_State to be not an incomplete type.

   --  We'd like to use Cursor here, but we want that to be tagged,
   --  to allow 'Cursor.Next' syntax, and the requirements of
   --  iterators prevent a tagged cursor type (two tagged types on
   --  First in body). So we use Parser_Node_Access as the iterator
   --  type for Iterators.

   type Parser_Node_Access is private;

   function To_Cursor
     (List : aliased in out Parser_Lists.List'Class;
      Ptr  :         in     Parser_Node_Access)
     return Cursor;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;

   function Has_Element (Cursor : in Parser_Node_Access) return Boolean;
   function Verb (Cursor : in Parser_Node_Access) return Parse_Action_Verbs;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class;

   ----------
   --  For unit tests, debug assertions

   function Parser_Free_Count (List : in Parser_Lists.List) return Integer;
   function Stack_Free_Count (List : in Parser_Lists.List) return Integer;
   function Action_Token_Free_Count (List : in Parser_Lists.List) return Integer;

   procedure Put (Action_Token : in Parser_Lists.Action_Token);
   procedure Put_Action_Tokens (Cursor : in Parser_Lists.Cursor);

   procedure Check_Action_Stack
     (Label  : in String;
      Cursor : in Parser_Lists.Cursor);
   --  Verify that all Action_Token.New_Token point to tokens on
   --  Stack or later Action_Token.Tokens, and that all
   --  nonterminals in Stack and Action_Token.Tokens are pointed to
   --  by previous Action_Token.New_Token.

private

   type Stack_Node;
   type Stack_Node_Access is access Stack_Node;
   type Stack_Node is record
      Item : Stack_Item;
      Next : Stack_Node_Access;
   end record;

   type Action_Token_Node;
   type Action_Token_Node_Access is access Action_Token_Node;
   type Action_Token_Node is record
      Item : Action_Token;
      Next : Action_Token_Node_Access;
      Prev : Action_Token_Node_Access;
   end record;

   type Action_Token_List is record
      Head : Action_Token_Node_Access;
      Tail : Action_Token_Node_Access;
      --  Enqueue to tail, dequeue from head, so 'prev', 'next' make sense
   end record;

   function Count (Action_Token : in Action_Token_List) return Integer;

   type Parser_State is record
      Label        : Integer;            -- for debugging
      Verb         : Parse_Action_Verbs; -- last action performed
      Stack        : Stack_Node_Access;
      Action_Token : Action_Token_List;
   end record;

   type Parser_Node;
   type Parser_Node_Access is access Parser_Node;

   type Parser_Node is record
      Item : aliased Parser_State;
      Next : Parser_Node_Access;
      Prev : Parser_Node_Access;
   end record;

   type List is tagged record
      Parser_Label      : Integer;
      Head              : Parser_Node_Access;
      Parser_Free       : Parser_Node_Access;
      Stack_Free        : Stack_Node_Access;
      Action_Token_Free : Action_Token_Node_Access;
      Count             : Integer;
   end record;

   type Cursor is tagged record
      List : access Parser_Lists.List;
      Ptr  : Parser_Node_Access;
   end record;


end OpenToken.Production.Parser.LALR.Parser_Lists;
