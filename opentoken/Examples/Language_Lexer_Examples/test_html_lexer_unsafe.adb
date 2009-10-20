-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephen Leake
-- Copyright (C) 1999, 2000 Christoph Karl Walter Grein
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
with OpenToken.Text_Feeder.Text_IO;
with HTML_Lexer.Task_Unsafe;
procedure Test_HTML_Lexer_Unsafe is

   use HTML_Lexer;
   use HTML_Lexer.Task_Unsafe;

   File  : Ada.Text_IO.File_Type;
   Token : HTML_Token;

begin

   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));

   Ada.Text_IO.Set_Input (File);

   Initialize (OpenToken.Text_Feeder.Text_IO.Create);

   loop

      Token := Next_Token;

      Ada.Text_IO.Put_Line
        ("Found " & Token_Name'Image (Name (Token)) &
           ' ' & Lexeme (Token));

      exit when Name (Token) = End_Of_File;

   end loop;

end Test_HTML_Lexer_Unsafe;
