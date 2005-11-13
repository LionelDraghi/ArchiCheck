-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: lookahead_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:22:30  Ted
-- analyzer lookahead test driver
--
--
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.String;
with OpenToken.Text_Feeder.Text_IO;

procedure Lookahead_Test.Run is
   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;

   File_Name : constant String := "Lookahead_Test.txt";

   Syntax : constant Tokenizer.Syntax :=
     (If_ID      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("if")),
      Then_ID    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("then")),
      Quit_ID    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("quit")),
      String_ID  => Tokenizer.Get(Opentoken.Recognizer.String.Get),
      Whitespace => Tokenizer.Get(Opentoken.Recognizer.Character_Set.Get
                                                      (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF => Tokenizer.Get(Opentoken.Recognizer.End_Of_File.Get)
      );

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);


begin


   ---------------------------------------------------------------------------
   -- Purpose          : Verify that the analyzer tokenizes properly in
   --                    lookahead mode.
   -- Input            : A string, followed by an "if", a "then" and a "quit"
   --                    token.
   -- Expected Results : The tokens should be returned in the correct order.
   --                    When non-lookahead calls to Find_Next resume, the
   --                    sequence of tokens should resume from right after
   --                    where it left off.
   ---------------------------------------------------------------------------
Case_1 :
    declare
       Text : constant String := """This is a standard WSIWG """"Ada"""" string \n.""if then quit";
       Passed : Boolean := True;
    begin

       Ada.Text_IO.Put ("Lookahead analyzing test...");
       Ada.Text_IO.Flush;

       Ada.Text_IO.Create
         (File => File,
          Mode => Ada.Text_IO.Out_File,
          Name => File_Name
          );

       Ada.Text_IO.Put_Line (File, Text);
       Ada.Text_IO.Close(File);

       Ada.Text_IO.Open
         (File => File,
          Mode => Ada.Text_IO.In_File,
          Name => File_Name
          );
       Ada.Text_IO.Set_Input (File);
       Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;


       Tokenizer.Find_Next (Analyzer);
       if Tokenizer.ID(Analyzer) /= String_ID then
          Passed := False;
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
          Ada.Text_IO.Put_Line ("  (Value = """ & Opentoken.Recognizer.String.Value
                                (Opentoken.Recognizer.String.Instance(Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a String_ID");

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer, Look_Ahead => True);

          if Tokenizer.ID(Analyzer) /= If_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(If_ID));

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer, Look_Ahead => True);

          if Tokenizer.ID(Analyzer) /= Then_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(Then_ID));

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer, Look_Ahead => True);

          if Tokenizer.ID(Analyzer) /= Quit_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(Quit_ID));

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer, Look_Ahead => True);

          if Tokenizer.ID(Analyzer) /= EOF then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting an end of file");

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer);

          if Tokenizer.ID(Analyzer) /= If_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(If_ID));

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer);

          if Tokenizer.ID(Analyzer) /= Then_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(Then_ID));

          end if;

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer);

          if Tokenizer.ID(Analyzer) /= Quit_ID then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image(Quit_ID));

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
          Ada.Text_IO.Put_Line ("Source string: " & Text);
    end Case_1;

end Lookahead_Test.Run;
