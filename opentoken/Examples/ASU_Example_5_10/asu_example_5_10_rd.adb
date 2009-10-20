-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Tags;

with OpenToken.Token.Linked_List;

-------------------------------------------------------------------------------
--  This example is a recursive-decent implementation of Example 5.10 from
--  "Compilers Principles, Techniques, and Tools" by Aho, Sethi, and Ullman
--  (aka: "The Dragon Book"). It demonstrates handling of synthesized
--  attributes.
-------------------------------------------------------------------------------
package body ASU_Example_5_10_RD is

   --------------------------------------------------------------------------
   --  Routine overloads for Our custom token types.
   --

   --------------------------------------------------------------------------
   --  Create an integer token using the given selected token.
   --------------------------------------------------------------------------
   overriding procedure Build
     (Match : in out Integer_Selection;
      From  : in     OpenToken.Token.Instance'Class)
   is begin
      if From in Integer_Token'Class then
         Match.Value := Integer_Token'Class (From).Value;
      elsif From in Integer_Literal.Class then
         Match.Value := Integer_Literal.Value (Integer_Literal.Class (From));
      else
         raise OpenToken.Parse_Error with "Unexpected token type """ &
           Ada.Tags.External_Tag (From'Tag) & """";
      end if;
   end Build;

   ----------------------------------------------------------------------------
   --  Overrides for the (implicitly) abstract "or" operations on
   --  Integer_Selection.
   ----------------------------------------------------------------------------
   overriding function "or"
     (Left  : access OpenToken.Token.Instance'Class;
      Right : access OpenToken.Token.Instance'Class)
     return Integer_Selection
   is begin
      return (Integer_Selection_Token."or" (Left, Right) with null record);
   end "or";

   overriding function "or"
     (Left  : access OpenToken.Token.Instance'Class;
      Right : in     Integer_Selection)
     return Integer_Selection
   is begin
      return (Integer_Selection_Token."or" (Left, Integer_Selection_Token.Instance (Right)) with null record);
   end "or";
   overriding function "or"
     (Left  : in     Integer_Selection;
      Right : access OpenToken.Token.Instance'Class)
     return Integer_Selection
   is begin
      return (Integer_Selection_Token."or" (Integer_Selection_Token.Instance (Left), Right) with null record);
   end "or";
   overriding function "or"
     (Left  : in Integer_Selection;
      Right : in Integer_Selection)
     return Integer_Selection
   is begin
      return (Integer_Selection_Token."or" (Integer_Selection_Token.Instance (Left),
                                            Integer_Selection_Token.Instance (Right)
                                            )
              with null record);
   end "or";

   --------------------------------------------------------------------------
   --  Build an integer token from an expression sequence.
   --  This should be a three-token sequence where the token we draw from is
   --  the second one.
   --------------------------------------------------------------------------
   overriding procedure Build (Match : in out Expression_Sequence)
   is
      Iterator : OpenToken.Token.Linked_List.List_Iterator :=
        OpenToken.Token.Linked_List.Initial_Iterator (Match.Members);
   begin
      OpenToken.Token.Linked_List.Next_Token (Iterator);
      Match.Value := Integer_Token_Handle (OpenToken.Token.Linked_List.Token_Handle (Iterator)).Value;
   end Build;

   ----------------------------------------------------------------------------
   --  Extensions for the (implicitly) abstract "&" functions.
   ----------------------------------------------------------------------------
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Expression_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Left, Right) with null record);
   end "&";
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Expression_Sequence)
     return Expression_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Left, Integer_Sequence_Token.Instance (Right)) with null record);
   end "&";
   overriding function "&"
     (Left  : in     Expression_Sequence;
      Right : access OpenToken.Token.Class)
     return Expression_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Integer_Sequence_Token.Instance (Left), Right) with null record);
   end "&";
   overriding function "&"
     (Left  : in Expression_Sequence;
      Right : in Expression_Sequence)
     return Expression_Sequence
   is begin
      return (Integer_Sequence_Token."&"
              (Integer_Sequence_Token.Instance (Left), Integer_Sequence_Token.Instance (Right))
              with null record);
   end "&";

   --------------------------------------------------------------------------
   --  Creation of tokens for mathematical operations.
   --------------------------------------------------------------------------
   overriding procedure Initialize (Match : in out Multiply_Operation_List) is
   begin
      Match.Value := 1;
   end Initialize;
   overriding procedure Add_List_Element
     (Match   : in out Multiply_Operation_List;
      Element : in out Integer_Token'Class
     ) is
   begin
      Match.Value := Match.Value * Element.Value;
   end Add_List_Element;
   overriding function Get
     (Element   : access Integer_Token'Class;
      Separator : access OpenToken.Token.Class
     ) return Multiply_Operation_List is
   begin
      return (Operation_List.Get
              (Element   => Element,
               Separator => Separator
               )
              with null record);
   end Get;

   overriding procedure Initialize (Match : in out Add_Operation_List) is
   begin
      Match.Value := 0;
   end Initialize;
   overriding procedure Add_List_Element
     (Match   : in out Add_Operation_List;
      Element : in out Integer_Token'Class
     ) is
   begin
      Match.Value := Match.Value + Element.Value;
   end Add_List_Element;

   overriding function Get
     (Element   : access Integer_Token'Class;
      Separator : access OpenToken.Token.Class
     ) return Add_Operation_List is
   begin
      return (Operation_List.Get
              (Element   => Element,
               Separator => Separator
               )
              with null record);
   end Get;

   --------------------------------------------------------------------------
   --  Build an integer token from an L sequence.
   --  This should be a two-token sequence where the token we draw from is
   --  the first one.
   --------------------------------------------------------------------------
   overriding procedure Build (Match : in out L_Sequence)
   is
      Iterator : constant OpenToken.Token.Linked_List.List_Iterator :=
        OpenToken.Token.Linked_List.Initial_Iterator (Match.Members);
   begin
      Match.Value := Integer_Token_Handle (OpenToken.Token.Linked_List.Token_Handle (Iterator)).Value;
      Ada.Text_IO.Put_Line (Integer'Image (Match.Value));
   end Build;

   ----------------------------------------------------------------------------
   --  Extensions for the (implicitly) abstract "&" functions.
   ----------------------------------------------------------------------------
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return L_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Left, Right) with null record);
   end "&";
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     L_Sequence)
     return L_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Left, Integer_Sequence_Token.Instance (Right)) with null record);
   end "&";
   overriding function "&"
     (Left  : in     L_Sequence;
      Right : access OpenToken.Token.Class)
     return L_Sequence
   is begin
      return (Integer_Sequence_Token."&"(Integer_Sequence_Token.Instance (Left), Right) with null record);
   end "&";
   overriding function "&"
     (Left  : in L_Sequence;
      Right : in L_Sequence)
     return L_Sequence
   is begin
      return (Integer_Sequence_Token."&"
              (Integer_Sequence_Token.Instance (Left), Integer_Sequence_Token.Instance (Right))
              with null record);
   end "&";

end ASU_Example_5_10_RD;
