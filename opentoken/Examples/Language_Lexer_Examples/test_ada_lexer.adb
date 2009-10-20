-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2008 Christoph Karl Walter Grein
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
with Ada.Command_Line;

with Ada_Lexer;
use  Ada_Lexer;

procedure Test_Ada_Lexer is

   --  Global text file for reading parse data
   File : Ada.Text_IO.File_Type;

begin

   Ada.Text_IO.Open (File => File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (1));

   Set_Input_Feeder (File);
   Bad_Token_on_Syntax_Error;

   loop

      Find_Next;

      Ada.Text_IO.Put_Line (Ada_Token'Image (Token_ID) & ' ' & Lexeme);

      exit when Token_ID = End_of_File_T;

   end loop;

end Test_Ada_Lexer;
