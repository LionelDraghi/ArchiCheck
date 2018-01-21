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
-----------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
package body OpenToken.Production.List is

   procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

   ----------------------------------------------------------------------------
   --  Create a production list from a single instance.
   ----------------------------------------------------------------------------
   function Only (Subject : in OpenToken.Production.Instance) return Instance is
      New_Node : constant List_Node_Ptr :=
        new List_Node'(Production => Subject,
                       Next       => null
                      );
   begin
      return (Head => New_Node,
              Tail => New_Node
             );
   end Only;

   ----------------------------------------------------------------------------
   --  Create a Production list from a pair of Production instances.
   ----------------------------------------------------------------------------
   function "and" (Left  : in OpenToken.Production.Instance;
                   Right : in OpenToken.Production.Instance) return Instance is
      Right_Node : constant List_Node_Ptr :=
        new List_Node'(Production => Right,
                       Next       => null
                      );
   begin
      return (Head => new List_Node'(Production => Left,
                                     Next  => Right_Node
                                    ),
              Tail => Right_Node
             );
   end "and";

   ----------------------------------------------------------------------------
   --  Create a Production list from a Production instance and a Production list.
   ----------------------------------------------------------------------------
   function "and" (Left  : in OpenToken.Production.Instance;
                   Right : in Instance) return Instance is
   begin
      return (Head => new List_Node'(Production => Left,
                                     Next       => Right.Head
                                    ),
              Tail => Right.Tail
             );
   end "and";

   function "and" (Left  : in Instance;
                   Right : in OpenToken.Production.Instance) return Instance is
      New_Node : constant List_Node_Ptr :=
        new List_Node'(Production => Right,
                       Next       => null
                      );
   begin
      Left.Tail.Next := New_Node;
      return (Head => Left.Head,
              Tail => New_Node
             );
   end "and";

   ----------------------------------------------------------------------------
   --  Create a Production list from a pair of Production lists.
   ----------------------------------------------------------------------------
   function "and" (Left  : in Instance;
                   Right : in Instance) return Instance is
   begin
      Left.Tail.Next := Right.Head;
      return (Head => Left.Head,
              Tail => Right.Tail
             );
   end "and";

   --------------------------------------------------------------------------
   --  This routine needs to be called when you are done using a list,
   --  or want to reset it to empty.
   --------------------------------------------------------------------------
   procedure Clean (List : in out Instance) is
      Node : List_Node_Ptr := List.Head;
      Next : List_Node_Ptr;
   begin
      --  Deallocate all the nodes in the list
      while Node /= null loop
         Next := Node.Next;
         Free (Node);
         Node := Next;
      end loop;

      List.Head := null;
      List.Tail := null;
   end Clean;

   ----------------------------------------------------------------------------
   --  Return an initialized iterator for traversing the Production list
   ----------------------------------------------------------------------------
   function Initial_Iterator (List : in Instance) return List_Iterator is
   begin
      return List_Iterator (List.Head);
   end Initial_Iterator;

   ----------------------------------------------------------------------------
   --  Move the iterator down the list to the next Production.
   ----------------------------------------------------------------------------
   procedure Next_Production (Iterator : in out List_Iterator) is
   begin
      if Iterator /= null then
         Iterator := List_Iterator (Iterator.Next);
      end if;
   end Next_Production;

   ----------------------------------------------------------------------------
   --  Return the next Production in the list.
   ----------------------------------------------------------------------------
   function Get_Production (Iterator : in List_Iterator) return OpenToken.Production.Instance is
   begin
      if Iterator = null then
         raise Iteration_Complete;
      end if;
      return Iterator.Production;
   end Get_Production;

   ----------------------------------------------------------------------------
   --  Return true if the iterator is at the last production.
   ----------------------------------------------------------------------------
   function Last_Production (Iterator : in List_Iterator) return Boolean is
   begin
      return Iterator = null or else Iterator.Next = null;
   end Last_Production;

   ----------------------------------------------------------------------------
   --  Return true if the iterator past the last production.
   ----------------------------------------------------------------------------
   function Past_Last (Iterator : in List_Iterator) return Boolean is
   begin
      return Iterator = null;
   end Past_Last;

end OpenToken.Production.List;
