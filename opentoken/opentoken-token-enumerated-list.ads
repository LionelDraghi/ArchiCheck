-------------------------------------------------------------------------------
--
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

-----------------------------------------------------------------------------
--  This package provides a type and operations for building lists of
--  tokens for use in grammar productions
-----------------------------------------------------------------------------
generic

package OpenToken.Token.Enumerated.List is

   type Instance is tagged private;

   Null_List : constant Instance;

   ----------------------------------------------------------------------------
   --  Create a token list from a single instance.
   ----------------------------------------------------------------------------
   function Only (Subject : in Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token list from a pair of token instances.
   ----------------------------------------------------------------------------
   function "&" (Left  : in Class;
                 Right : in Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token list from a token instance and a token list.
   ----------------------------------------------------------------------------
   function "&" (Left  : in Class;
                 Right : in Instance) return Instance;
   function "&" (Left  : in Instance;
                 Right : in Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token list from a pair of token lists.
   ----------------------------------------------------------------------------
   function "&" (Left  : in Instance;
                 Right : in Instance) return Instance;

   --------------------------------------------------------------------------
   --  This routine needs to be called when you are done using a list,
   --  or want to reset it to empty.
   --------------------------------------------------------------------------
   procedure Clean (List : in out Instance);

   type List_Iterator is private;
   Null_Iterator : constant List_Iterator;

   ----------------------------------------------------------------------------
   --  Return an initialized iterator for traversing the token list
   ----------------------------------------------------------------------------
   function Initial_Iterator (List : in Instance) return List_Iterator;

   ----------------------------------------------------------------------------
   --  Move the iterator down the list to the next token.
   ----------------------------------------------------------------------------
   procedure Next_Token (Iterator : in out List_Iterator);

   ----------------------------------------------------------------------------
   --  Return the next Token in the list.
   ----------------------------------------------------------------------------
   function Token_Handle (Iterator : in List_Iterator) return Handle;

   ---------------------
   -- Parser Routines --

   --------------------------------------------------------------------------
   --  Enqueue a token on the given list. The token itself will not be
   --  copied.  Its storage will be managed by the list from here on
   --  in. Do not delete it while the list is still using it!
   --------------------------------------------------------------------------
   procedure Enqueue (List  : in out Instance;
                      Token : in     Handle
                     );

private
   type List_Node;
   type List_Node_Ptr is access List_Node;
   type List_Node is record
      Token : Handle;
--       Count : Natural;
      Next  : List_Node_Ptr;
   end record;

   type Instance is tagged record
     Head : List_Node_Ptr;
     Tail : List_Node_Ptr;
   end record;

   type List_Iterator is new List_Node_Ptr;
   Null_Iterator : constant List_Iterator := null;

   Null_List : constant Instance := (null, null);

end OpenToken.Token.Enumerated.List;
