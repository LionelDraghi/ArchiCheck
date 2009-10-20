-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000,2009 Ted Dennison
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

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;
procedure String_Test.Run is

   ---------------------------------------------------------------------------
   --  Purpose          : Verify that a valid Ada string is read correctly.
   --  Input            : A string with embedded doubled quotations and a C
   --                     escape sequence. A valid token immediately after
   --  Expected Results : The same string with the double quotes replaced by
   --                     single qoutes. Then the next Opentoken.Recognizer.
   ---------------------------------------------------------------------------
   procedure Case_1
   is
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
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name
        );
      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;


      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= String_ID then
         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
         Ada.Text_IO.Put_Line ("  (Value = """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (Ada_Syntax (String_ID).Recognizer.all)) &
                                 """)");
         Ada.Text_IO.Put_Line ("when expecting a String_ID");

      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);

         if Tokenizer.ID (Analyzer) /= If_ID then

            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image (If_ID));

         end if;

      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);

         if Tokenizer.ID (Analyzer) /= EOF then

            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line (" when expecting an end of file");

         end if;

      end if;

      if Passed then
         Ada.Text_IO.Put_Line ("passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Ada.Text_IO.Close (File);

   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed.");
      Ada.Text_IO.Put_Line ("Exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      Ada.Text_IO.Put_Line ("Source string: " & Text);
   end Case_1;

   ---------------------------------------------------------------------------
   --  Purpose          : Verify that an invalid Ada string is read correctly.
   --  Input            : A string with embedded doubled quotations and a
   --                     missing end quotation.
   --  Expected Results : A syntax error.
   ---------------------------------------------------------------------------
   procedure Case_2
   is
      Text : constant String := """This is an """"Ada"""" string w/o an end quotation" &
        OpenToken.EOL_Character;
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
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name
        );
      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;


      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= String_ID then
         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
         Ada.Text_IO.Put_Line ("  (Value = """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (Ada_Syntax (String_ID).Recognizer.all)) &
                                 """)");
         Ada.Text_IO.Put_Line ("when expecting a String_ID");

      end if;

      if Passed then
         begin
            Tokenizer.Find_Next (Analyzer);

            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put_Line ("  (Value = """ & OpenToken.Recognizer.String.Value
                                    (OpenToken.Recognizer.String.Instance (Ada_Syntax (String_ID).Recognizer.all)) &
                                    """)");
            Ada.Text_IO.Put_Line ("when expecting a Syntax Error");
         exception
         when OpenToken.Syntax_Error =>
            Ada.Text_IO.Put_Line ("passed.");
         end;
      end if;

      if Passed then
         Ada.Text_IO.Put_Line ("passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Ada.Text_IO.Close (File);

   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed.");
      Ada.Text_IO.Put_Line ("Exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      Ada.Text_IO.Put_Line ("Source string: " & Text);
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
   end Case_2;

   ---------------------------------------------------------------------------
   --  Purpose          : Verify that a valid C string is read correctly.
   --  Input            : A string with embedded C escape sequences. A valid
   --                     token immediately after.
   --  Expected Results : The same string with the escapes properly replaced.
   --                     Then the next token.
   ---------------------------------------------------------------------------
   procedure Case_3
   is
      Text : constant String := """This is a standard \""C\"" string \n.""if";
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
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name
        );
      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;


      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= String_ID then
         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
         Ada.Text_IO.Put_Line ("  (Value = """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) & """)");
         Ada.Text_IO.Put_Line ("when expecting a String_ID");

      elsif OpenToken.Recognizer.String.Value
        (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) /= Expected_Result
      then

         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) & '"');
         Ada.Text_IO.Put_Line
           ("(" & Integer'Image
              (OpenToken.Recognizer.String.Value (OpenToken.Recognizer.String.Instance
                                                    (C_Syntax (String_ID).Recognizer.all))'Length) &
              " characters)");
      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);

         if Tokenizer.ID (Analyzer) /= If_ID then

            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line (" when expecting a " & Example_Token_ID'Image (If_ID));

         end if;

      end if;

      if Passed then
         Tokenizer.Find_Next (Analyzer);

         if Tokenizer.ID (Analyzer) /= EOF then

            Passed := False;
            Ada.Text_IO.Put_Line ("failed.");
            Ada.Text_IO.Put ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
            Ada.Text_IO.Put (" (""" & Tokenizer.Lexeme (Analyzer) & """)");
            Ada.Text_IO.Put_Line (" when expecting an end of file");

         end if;

      end if;

      if Passed then
         Ada.Text_IO.Put_Line ("passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Ada.Text_IO.Close (File);

   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed.");
      Ada.Text_IO.Put_Line ("Exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      Ada.Text_IO.Put_Line ("Source string: " & Text);
   end Case_3;

   ---------------------------------------------------------------------------
   --  Purpose          : Verify that a C string with consecutive escaped
   --                     characters is read correctly.
   --  Input            : Two C escape sequences right next to each other.
   --  Expected Results : The same string with the escaped strings properly
   --                     replaced.
   ---------------------------------------------------------------------------
   procedure Case_4
   is
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
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name
        );
      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;


      Tokenizer.Find_Next (Analyzer);
      if Tokenizer.ID (Analyzer) /= String_ID then
         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
         Ada.Text_IO.Put_Line ("  (Value = """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) & """)");
         Ada.Text_IO.Put_Line ("when expecting a String_ID");

      elsif OpenToken.Recognizer.String.Value
        (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) /= Expected_Result
      then

         Passed := False;
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Found """ & OpenToken.Recognizer.String.Value
                                 (OpenToken.Recognizer.String.Instance (C_Syntax (String_ID).Recognizer.all)) & '"');
         Ada.Text_IO.Put_Line
           ("(" & Integer'Image (OpenToken.Recognizer.String.Value (OpenToken.Recognizer.String.Instance
                                                                      (C_Syntax (String_ID).Recognizer.all))'Length) &
              " characters)");
      end if;

      if Passed then
         Ada.Text_IO.Put_Line ("passed.");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Ada.Text_IO.Close (File);

   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed.");
      Ada.Text_IO.Put_Line ("Exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      Ada.Text_IO.Put_Line ("Source string: " & Text);
   end Case_4;
begin
   Case_1;
   Case_2;
   Case_3;
   Case_4;
end String_Test.Run;
