-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000 Ted Dennison
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
-- $Log: string_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:15:55  Ted
-- moved from string_test.adb
--
-- Revision 1.4  2000/02/05 04:06:21  Ted
-- Add test run for two consecutive octal escape sequences.
--
-- Revision 1.3  2000/01/27 21:18:20  Ted
-- Fix to work with 2.0
--
-- Revision 1.2  1999/12/27 19:56:05  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/08/17 03:37:37  Ted
-- Initial Version
--
--
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;

procedure String_Test.Run is
begin


   ---------------------------------------------------------------------------
   -- Purpose          : Verify that a valid Ada string is read correctly.
   -- Input            : A string with embedded doubled quotations and a C
   --                    escape sequence. A valid token immediately after
   -- Expected Results : The same string with the double quotes replaced by
   --                    single qoutes. Then the next Opentoken.Recognizer.
   ---------------------------------------------------------------------------
Case_1 :
    declare
       Text : constant String := """This is a standard WSIWG """"Ada"""" string \n.""if";
       Passed : Boolean := True;
    begin

       Ada.Text_IO.Put ("Valid Ada string test...");
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
                                (Opentoken.Recognizer.String.Instance(Ada_Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a String_ID");

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

          if Tokenizer.ID(Analyzer) /= EOF then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting an end of file");

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

   ---------------------------------------------------------------------------
   -- Purpose          : Verify that an invalid Ada string is read correctly.
   -- Input            : A string with embedded doubled quotations and a
   --                    missing end quotation.
   -- Expected Results : A syntax error.
   ---------------------------------------------------------------------------
Case_2 :
    declare
       Text : constant String := """This is an """"Ada"""" string w/o an end quotation" &
         Opentoken.EOL_Character;
       Passed : Boolean := True;
    begin

       Ada.Text_IO.Put ("Inalid Ada string test...");
       Ada.Text_IO.Flush;

       Ada.Text_IO.Open
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
                                (Opentoken.Recognizer.String.Instance(Ada_Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a String_ID");

       end if;

       if Passed then
          Tokenizer.Find_Next (Analyzer);

          Passed := False;
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
          Ada.Text_IO.Put_Line ("  (Value = """ & Opentoken.Recognizer.String.Value
                                (Opentoken.Recognizer.String.Instance(Ada_Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a Syntax Error");
       end if;

       Ada.Text_IO.Close(File);

    exception
       when OpenToken.Syntax_Error =>
          Ada.Text_IO.Put_Line ("passed.");
          Ada.Text_IO.Close(File);
       when Error : others =>
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Exception:");
          Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
          Ada.Text_IO.Put_Line ("Source string: " & Text);
          if Ada.Text_IO.Is_Open(File) then
             Ada.Text_IO.Close(File);
          end if;
    end Case_2;

   ---------------------------------------------------------------------------
   -- Purpose          : Verify that a valid C string is read correctly.
   -- Input            : A string with embedded doubled quotations and a C
   --                    escape sequence. A valid token immediately after
   -- Expected Results : The same string with the double quotes replaced by
   --                    single qoutes and the escaped strings properly
   --                    replaced. Then the next token.
   ---------------------------------------------------------------------------
Case_3 :
    declare
       Text : constant String := """This is a standard """"C"""" string \n.""if";
       Expected_Result : constant String := "This is a standard ""C"" string " & Ada.Characters.Latin_1.LF & '.';
       Passed : Boolean := True;
       Analyzer : Tokenizer.Instance := Tokenizer.Initialize (C_Syntax);

    begin

       Ada.Text_IO.Put ("Valid C string test...");
       Ada.Text_IO.Flush;

       Ada.Text_IO.Open
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
                                (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a String_ID");

       elsif Opentoken.Recognizer.String.Value
         (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) /= Expected_Result
       then

          Passed := False;
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Found """ & Opentoken.Recognizer.String.Value
                                (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) & '"');
          Ada.Text_IO.Put_Line ("(" & Integer'Image (Opentoken.Recognizer.String.Value(Opentoken.Recognizer.String.Instance
                                                                         (C_Syntax(String_ID).Recognizer.all))'Length) &
                                " characters)");
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

          if Tokenizer.ID(Analyzer) /= EOF then

             Passed := False;
             Ada.Text_IO.Put_Line ("failed.");
             Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
             Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme(Analyzer) & """)");
             Ada.Text_IO.Put_Line (" when expecting an end of file");

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
    end Case_3;

   ---------------------------------------------------------------------------
   -- Purpose          : Verify that a C string with consecutive escaped
   --                    characters is read correctly.
   -- Input            : Two C escape sequences right next to each other.
   -- Expected Results : The same string with the escaped strings properly
   --                    replaced.
   ---------------------------------------------------------------------------
Case_4 :
    declare
       Text : constant String := """\074\075f""";
       Expected_Result : constant String := "<=f";
       Passed : Boolean := True;
       Analyzer : Tokenizer.Instance := Tokenizer.Initialize (C_Syntax);

    begin

       Ada.Text_IO.Put ("Double-escaped string test...");
       Ada.Text_IO.Flush;

       Ada.Text_IO.Open
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
                                (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) & """)");
          Ada.Text_IO.Put_Line ("when expecting a String_ID");

       elsif Opentoken.Recognizer.String.Value
         (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) /= Expected_Result
       then

          Passed := False;
          Ada.Text_IO.Put_Line ("failed.");
          Ada.Text_IO.Put_Line ("Found """ & Opentoken.Recognizer.String.Value
                                (Opentoken.Recognizer.String.Instance(C_Syntax(String_ID).Recognizer.all)) & '"');
          Ada.Text_IO.Put_Line ("(" & Integer'Image (Opentoken.Recognizer.String.Value(Opentoken.Recognizer.String.Instance
                                                                         (C_Syntax(String_ID).Recognizer.all))'Length) &
                                " characters)");
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
    end Case_4;

end String_Test.Run;
