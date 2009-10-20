-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2000 Ted Dennison
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

with Ada.Unchecked_Deallocation;
package body OpenToken.Token.Linked_List is

   procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

   function Only (Subject : in OpenToken.Token.Handle) return Instance is
      New_Node : constant List_Node_Ptr :=
        new List_Node'(Token => Subject,
                       Count => 1,
                       Next  => null
                       );
   begin
      return (Ada.Finalization.Controlled with
              Head => New_Node,
              Tail => New_Node
              );
   end Only;

   function "&" (Left  : in OpenToken.Token.Handle;
                 Right : in OpenToken.Token.Handle) return Instance is
      Right_Node : constant List_Node_Ptr :=
        new List_Node'(Token => Right,
                       Count => 1,
                       Next  => null
                       );
   begin
      return (Ada.Finalization.Controlled with
              Head => new List_Node'(Token => Left,
                                     Count => 1,
                                     Next  => Right_Node
                                     ),
              Tail => Right_Node
              );
   end "&";


   function "&"
     (Left  : in OpenToken.Token.Handle;
      Right : in Instance)
     return Instance
   is
      Left_Node : constant List_Node_Ptr :=
        new List_Node'(Token => Left,
                       Count => 1,
                       Next  => Right.Head
                       );
      Last_Node : List_Node_Ptr := Right.Tail;
   begin
      Right.Head.Count := Right.Head.Count + 1;

      if Last_Node = null then
         Last_Node := Left_Node;
      end if;

      return (Ada.Finalization.Controlled with
              Head => Left_Node,
              Tail => Last_Node
              );
   end "&";

   function "&" (Left  : in Instance;
                 Right : in OpenToken.Token.Handle) return Instance is
      New_Node : constant List_Node_Ptr :=
        new List_Node'(Token => Right,
                       Count => 1,
                       Next  => null
                       );
      First_Node : List_Node_Ptr;
   begin
      Left.Head.Count := Left.Head.Count + 1;

      if Left.Tail = null then
         First_Node := New_Node;
      else
         First_Node     := Left.Head;
         Left.Tail.Next := New_Node;
      end if;
      return (Ada.Finalization.Controlled with
              Head => First_Node,
              Tail => New_Node
              );
   end "&";

   function "&" (Left  : in Instance;
                 Right : in Instance) return Instance is
   begin
      Left.Tail.Next := Right.Head;
      Left.Head.Count :=  Left.Head.Count + 1;
      Right.Head.Count := Right.Head.Count + 1;

      return (Ada.Finalization.Controlled with
              Head => Left.Head,
              Tail => Right.Tail
              );
   end "&";

   procedure Enqueue
     (List  : in out Instance;
      Token : in     OpenToken.Token.Handle)
   is
      New_Node : constant List_Node_Ptr :=
        new List_Node'(Token => Token,
                       Count => 1,
                       Next  => List.Head
                       );
   begin
      if List.Tail = null then
         List.Tail := New_Node;
      end if;
      List.Head := New_Node;
   end Enqueue;

   procedure Clean (List : in out Instance) is
      Node : List_Node_Ptr := List.Head;
      Next : List_Node_Ptr;
   begin
      --  Deallocate all the nodes in the list, along with all their tokens
      while Node /= null loop
         Next := Node.Next;
         Free (Node);
         Node := Next;
      end loop;

      List.Head := null;
      List.Tail := null;
   end Clean;

   --------------------------------------------------------------------------
   --  A controlled object gets adjusted during an assignment,
   --  immediately after making a bit-wise copy from the value on the
   --  right hand side to the object on the left hand side.
   --
   --  When we assign one list object to another, we have to increment
   --  the reference count, because there is now one more object
   --  referring to that node
   --
   ----------------------------------------------------------------------------
   overriding procedure Adjust (List : in out Instance) is
   begin
      if List.Head /= null then
         List.Head.Count := List.Head.Count + 1;
      end if;
   end Adjust;

   --------------------------------------------------------------------------
   --  Dereferencing of one node can cause a chain reaction of node
   --  dereferences to occur. When the reference count for a node
   --  drops to zero, you have to return the node to storage. That
   --  means there's one less reference to the next node, so that next
   --  node needs to have its reference count decremented. But if its
   --  count goes to zero, then you have to dereference its next node,
   --  and so on.
   --
   --  The recursion terminates when you decrement a node whose
   --  reference count is greater than one, or you fall off the end of
   --  the list (because the reference counts of all the nodes in the
   --  list were one).
   --
   ----------------------------------------------------------------------------
   procedure Dereference (Node : in out List_Node_Ptr) is
   begin

      Node.Count := Node.Count - 1;

      if Node.Count = 0 then

         if Node.Next /= null then
            Dereference (Node.Next);
         end if;

         --  Free the dynamicly allocated memory
         Free (Node);

      end if;

   end Dereference;

   --------------------------------------------------------------------------
   --  A controlled object gets finalized immediately prior to the
   --  bit-wise copy, and when the scope in which the object is
   --  declared ends. When a list object is finalized, it means there
   --  is one less list object pointing to the first node, so you have
   --  to decrement the reference Count.
   --
   ----------------------------------------------------------------------------
   overriding procedure Finalize (List : in out Instance) is
   begin
      if List.Head /= null then
         Dereference (List.Head);

         if List.Head = null then
            List.Tail := null;
         end if;
      end if;
   end Finalize;

   function Initial_Iterator (List : in Instance) return List_Iterator is
   begin
      return List_Iterator (List.Head);
   end Initial_Iterator;

   procedure Next_Token (Iterator : in out List_Iterator) is
   begin
      if Iterator /= null then
         Iterator := List_Iterator (Iterator.Next);
      end if;
   end Next_Token;

   function Token_Handle (Iterator : in List_Iterator) return OpenToken.Token.Handle is
   begin
      if Iterator = null then
         return null;
      end if;
      return Iterator.Token;
   end Token_Handle;

end OpenToken.Token.Linked_List;
