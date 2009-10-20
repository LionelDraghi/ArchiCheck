-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 1999 Christoph Karl Walter Grein
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;
procedure Bracketed_Comment_Test.Run is

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);

   --  Global text file for reading parse data
   File : File_Type;

begin

   ---------------------------------------------------------------------------
   --  Purpose          : Verify that a valid bracketed comment is read
   --                     correctly.
   --  Input            : 3 Bracketed comments with long terminators.
   --  Expected Results : 3 Bracketed comment tokens
   ---------------------------------------------------------------------------
   Case_1 :
   declare
      Text1 : constant String := "/* A comment that ends here *.*..";
      Text2 : constant String := "/* Another comment that ends a bit later *.*.*.*..";
      Text3 : constant String := "/* It ends here *.*..";

      File_Name : constant String := "Bracketed_Comment_Test_1.txt";

      Passed : Boolean := True;
   begin
      Put ("Valid bracketed comment test 1...");
      Flush;

      Create
        (File => File,
         Mode => Out_File,
         Name => File_Name
        );
      Put_Line (File, Text1);
      Put_Line (File, Text2);
      Put_Line (File, (1 .. 30 => ' ') & Text3);
      Close (File);

      Open
        (File => File,
         Mode => In_File,
         Name => File_Name);

      Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme (Analyzer) /= Text1 then
         Passed := False;
         Put_Line ("failed.");
         Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
         Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
         Put_Line ("when expecting an EmbeddedComment_T");
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID (Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme (Analyzer) /= Text2 then
            Passed := False;
            Put_Line ("failed.");
            Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Put_Line ("when expecting an EmbeddedComment_T");
         end if;
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID (Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme (Analyzer) /= Text3 then
            Passed := False;
            Put_Line ("failed.");
            Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Put_Line ("when expecting an EmbeddedComment_T");
         end if;
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID (Analyzer) /= End_Of_File_T then
            Passed := False;
            Put_Line ("failed.");
            Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Put_Line ("when expecting an End_Of_File_T");
         end if;
      end if;

      if Passed then
         Put_Line ("1 passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Close (File);
   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line ("failed.");
      Put_Line ("Exception:");
      Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Case_1;

   --  Multi-line comment
   Case_2 :
   declare
      Text1 : constant String := "/* A comment that starts here";
      Text2 : constant String := "   and keeps going";
      Text3 : constant String := "   and finally ends here *.*..";

      Expected_Lexeme : constant String := Text1 & OpenToken.EOL_Character & Text2 & OpenToken.EOL_Character & Text3;

      File_Name : constant String := "Bracketed_Comment_Test_2.txt";

      Passed : Boolean := True;
   begin
      Put ("Valid bracketed comment test 2...");
      Flush;

      Create (File, Out_File, File_Name);
      Put_Line (File, Text1);
      Put_Line (File, Text2);
      Put_Line (File, Text3);
      Close (File);

      Open (File, In_File, File_Name);

      Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme (Analyzer) /= Expected_Lexeme then
         Passed := False;
         Put_Line ("failed.");
         Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
         Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
         Put_Line ("when expecting");
         Put_Line ("  (Value = """ & Expected_Lexeme & """)");
      end if;

      if Passed then
         Put_Line ("passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;


      Close (File);
   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line ("failed.");
      Put_Line ("Exception:");
      Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Case_2;

end Bracketed_Comment_Test.Run;
