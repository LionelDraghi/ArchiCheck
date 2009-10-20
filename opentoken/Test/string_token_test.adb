--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2006 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Ada.Text_IO;
package body String_Token_Test is

   procedure Synthesize_Display
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type)
   is
      I : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source); --  String
   begin
      New_Token := Nonterminal.Instance'Class
        (Nonterminal.Instance'(Master_Token.Instance (Master_Token.Get (To_ID)) with null record));

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("String => '" &
           String_Literal.Value (String_Literal.Instance (Token_List.Token_Handle (I).all)) &
           "'");
   end Synthesize_Display;

end String_Token_Test;

