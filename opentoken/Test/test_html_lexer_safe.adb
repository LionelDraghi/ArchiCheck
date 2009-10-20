--  Abstract :
--
--  Test HTML_Lexer
--
--  Copyright (C) 2003, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with HTML_Lexer.Task_Safe;
with OpenToken.Text_Feeder.Text_IO;
procedure Test_HTML_Lexer_Safe
is
   procedure Put_Usage
   is begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "usage: test_html_lexer file");
   end Put_Usage;

   File_Name   : Ada.Strings.Unbounded.Unbounded_String;
   File        : aliased Ada.Text_IO.File_Type;
   Lexer       : HTML_Lexer.Task_Safe.Lexer_Type;
   Text_Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance;

   procedure Put_Token (Token : in HTML_Lexer.HTML_Token)
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use HTML_Lexer;
   begin
      Ada.Text_IO.Put_Line
        (To_String (File_Name) &
           ":" &
           Trim (Natural'Image (Line (Token)), Both) &
           ":" &
           Trim (Natural'Image (Column (Token)), Both) &
           ": " &
           Token_Name'Image (Name (Token)) & ": " & Lexeme (Token));
   end Put_Token;

   procedure Parse_File
   is
      use HTML_Lexer;
      Token : HTML_Token;
   begin
      loop
         HTML_Lexer.Task_Safe.Next_Token (Lexer, Token);
         Put_Token (Token);

         exit when Name (Token) = End_Of_File;
      end loop;
   end Parse_File;

begin
   if Ada.Command_Line.Argument_Count = 1 then
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Ada.Command_Line.Argument (1));
   else
      Put_Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   File_Name := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Command_Line.Argument (1));

   Text_Feeder := OpenToken.Text_Feeder.Text_IO.Create (File'Unchecked_Access);
   HTML_Lexer.Task_Safe.Initialize (Lexer, Text_Feeder'Unchecked_Access);

   Parse_File;

end Test_HTML_Lexer_Safe;
