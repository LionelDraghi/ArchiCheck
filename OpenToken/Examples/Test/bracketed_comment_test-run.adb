-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
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
-- $Log: bracketed_comment_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:27:22  Ted
-- moved from bracketed_comment_test.adb
--
-- Revision 1.1  2000/02/05 04:04:34  Ted
-- Test driver for bracketed comment support.
--
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with OpenToken.Text_Feeder.Text_IO;

procedure Bracketed_Comment_Test.Run is

  Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);

  -- Global text file for reading parse data
  File      : Ada.Text_IO.File_Type;
  File_Name : constant String := "Bracketed_Comment_Test.txt";


begin

   ---------------------------------------------------------------------------
   -- Purpose          : Verify that a valid bracketed comment is read
   --                    correctly.
   -- Input            : 3 Bracketed comments with long terminators.
   -- Expected Results : 3 Bracketed comment tokens
   ---------------------------------------------------------------------------
Case_1 :
   declare
      Text1 : constant String := "/* A comment that ends here *.*..";
      Text2 : constant String := "/* Another comment that ends a bit later *.*.*.*..";
      Text3 : constant String := "/* It ends here *.*..";

      Passed : Boolean := True;
   begin
      Ada.Text_IO.Put ("Valid bracketed comment test...");
      Ada.Text_IO.Flush;

      Ada.Text_IO.Create
        (File => File,
         Mode => Ada.Text_IO.Out_File,
         Name => File_Name
         );
      Ada.Text_IO.Put_Line (File, Text1);
      Ada.Text_IO.Put_Line (File, Text2);
      Ada.Text_IO.Put_Line (File, (1..30 => ' ') & Text3);
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name);

      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID(Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme(Analyzer) /= Text1 then
         Passed := False;
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
          Ada.Text_IO.Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
          Ada.Text_IO.Put_Line ("when expecting an EmbeddedComment_T");
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID(Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme(Analyzer) /= Text2 then
            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line ("when expecting an EmbeddedComment_T");
         end if;
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID(Analyzer) /= EmbeddedComment_T or Tokenizer.Lexeme(Analyzer) /= Text3 then
            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line ("when expecting an EmbeddedComment_T");
         end if;
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);
         if Tokenizer.ID(Analyzer) /= End_Of_File_T then
            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put_Line ("Found " & Test_Token'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put_Line ("  (Value = """ & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line ("when expecting an End_Of_File_T");
         end if;
      end if;

       if Passed then
          Ada.Text_IO.Put_Line ("passed.");
        end if;


       Ada.Text_IO.Close(File);
    exception
       when Error : others =>
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Exception:");
          Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
   end Case_1;

end Bracketed_Comment_Test.Run;

