--  Abstract :
--
--  Run String_Token_Test
--
--  Copyright (C) 2002, 2003, 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
procedure String_Token_Test.Run
is
   use LALR_Parser;

   Trace : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   procedure Parse_Statement (Parser : in out LALR_Parser.Instance; Statement : in String)
   is begin
      Put_Line ("'" & Statement & "'");
      OpenToken.Text_Feeder.String.Set (String_Feeder, Statement);

      Set_Text_Feeder (Parser, String_Feeder'Unchecked_Access);

      --  Read and parse statements from the string until end of string
      loop
         exit when End_Of_Text (Parser);

         Parse (Parser);
      end loop;
      Put_Line ("success");
      New_Line;
   exception
   when E : others =>
      Discard_Buffered_Text (Parser);
      Put_Line ("error: " & Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
      New_Line;
   end Parse_Statement;

   Parser : LALR_Parser.Instance := LALR_Parser.Generate (Grammar, The_Analyzer, Trace => Trace);
begin
   if Trace then
      LALR_Parser.Print_Table (Parser);
   end if;

   LALR_Parser.Set_Trace (Parser, True);

   Parse_Statement (Parser, " ""foo"" ");
   Parse_Statement (Parser, """Module""""Component""");
   Parse_Statement (Parser, " """" ");

end String_Token_Test.Run;
