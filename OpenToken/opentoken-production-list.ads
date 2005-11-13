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
-- $Log: opentoken-production-list.ads,v $
-- Revision 1.1  2000/01/27 20:51:11  Ted
-- Lists of productions
--
--
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package provides a type and operations for building lists of
-- productions for use in grammars.
-------------------------------------------------------------------------------
generic package OpenToken.Production.List is

   type Instance is tagged private;

   ----------------------------------------------------------------------------
   -- Create a production list from a single instance.
   ----------------------------------------------------------------------------
   function Only (Subject : in OpenToken.Production.Instance) return Instance;

   ----------------------------------------------------------------------------
   -- Create a production list from a pair of production instances.
   ----------------------------------------------------------------------------
   function "and" (Left  : in OpenToken.Production.Instance;
                   Right : in OpenToken.Production.Instance) return Instance;

   ----------------------------------------------------------------------------
   -- Create a production list from a production instance and a production list.
   ----------------------------------------------------------------------------
   function "and" (Left  : in OpenToken.Production.Instance;
                   Right : in Instance) return Instance;
   function "and" (Left  : in Instance;
                   Right : in OpenToken.Production.Instance) return Instance;

   ----------------------------------------------------------------------------
   -- Create a token list from a pair of token lists.
   ----------------------------------------------------------------------------
   function "and" (Left  : in Instance;
                   Right : in Instance) return Instance;

   ----------------------------------------------------------------------------
   -- This routine needs to be called when you are done using a list, or want
   -- to reset it to empty.
   ----------------------------------------------------------------------------
   procedure Clean (List : in out Instance);

   type List_Iterator is private;

   Iteration_Complete : exception;

   ----------------------------------------------------------------------------
   -- Return an initialized iterator for traversing the token list
   ----------------------------------------------------------------------------
   function Initial_Iterator (List : in Instance) return List_Iterator;

   ----------------------------------------------------------------------------
   -- Move the iterator down the list to the next token.
   ----------------------------------------------------------------------------
   procedure Next_Production (Iterator : in out List_Iterator);

   ----------------------------------------------------------------------------
   -- Return the next Token in the list.
   ----------------------------------------------------------------------------
   function Get_Production (Iterator : in List_Iterator) return OpenToken.Production.Instance;

   ----------------------------------------------------------------------------
   -- Return true if the iterator is at the last production.
   ----------------------------------------------------------------------------
   function Last_Production (Iterator : in List_Iterator) return Boolean;

   ----------------------------------------------------------------------------
   -- Return true if the iterator past the last production.
   ----------------------------------------------------------------------------
   function Past_Last (Iterator : in List_Iterator) return Boolean;

private
   type List_Node;
   type List_Node_Ptr is access List_Node;
   type List_Node is record
      Production : OpenToken.Production.Instance;
      Next       : List_Node_Ptr;
   end record;

   type Instance is tagged record
     Head : List_Node_Ptr;
     Tail : List_Node_Ptr;
   end record;

   type List_Iterator is new List_Node_Ptr;

end OpenToken.Production.List;
