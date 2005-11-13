-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2000 Christoph Karl Walter Grein
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
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
-- $Log: test_html_lexer.adb,v $
-- Revision 1.2  2000/08/07 00:15:26  Ted
-- Update to use new string feeder
--
-- Revision 1.1  1999/12/27 21:41:56  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/22  Grein
-- First release;
--
-- Revision 0.0  1999/12/16  Grein
-- Initial Version
--
-------------------------------------------------------------------------------

with Ada.Text_Io;
with Ada.Command_Line;

with OpenToken.Text_Feeder.Text_IO;

with HTML_Lexer;
use  HTML_Lexer;

procedure Test_HTML_Lexer is

  File : Ada.Text_IO.File_Type;
  Token: HTML_Token;

begin

  Ada.Text_IO.Open
    (File => File,
     Mode => Ada.Text_IO.In_File,
     Name => Ada.Command_Line.Argument (1));
  Ada.Text_IO.Set_Input (File);

  Initialize (OpenToken.Text_Feeder.Text_IO.Create);

  loop

    Token := Next_Token;

    Ada.Text_Io.Put_Line ("Found " & Token_Name'Image (Name (Token)) &
                          ' ' & Lexeme (Token));

    exit when Name (Token) = End_Of_File;

  end loop;

end Test_HTML_Lexer;
