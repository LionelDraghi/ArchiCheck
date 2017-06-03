-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

-- with OpenToken.Recognizer;
with OpenToken.Token.Sequence; use OpenToken.Token;
with OpenToken.Token.Selection;
with OpenToken.Token.List;
with OpenToken.Text_Feeder.Text_IO;
--use type OpenToken.Token.Sequence.Instance;
--use type OpenToken.Token.Selection.Instance;

--use Lexer;

procedure Archicheck.Rules_Parser.Parse is
   File : Ada.Text_IO.File_Type;
   File_Name : constant String := Ada.Command_Line.Argument (1);
   Input_File : aliased Ada.Text_IO.File_Type;

begin

   --     Sentence.all := OpenToken.Token.Selection.Class
--       (Lexer.Component_Def or Lexer.Comment_T or Lexer.EoL_T);
--     Component_Def.all := OpenToken.Token.Sequence.Class
--       (Lexer.Identifier_T & Lexer.Contains_T & Lexer.Identifier_T);
   -- Component definition : "Component contains Unit"

   -- Units_In_File definition : "units in Dir1/*.ads"
   --     Units_In_File.all := Selection.Class
   --       (File or
   --        Sequence.New_Instance (File & Selection.Class (And_T or Comma_T)));
   -- File_List : File [(and|,) File]
   -- example : units in Dir1/*.ads, units in Dir2 and units in Dir3"

   declare
      --use Lexer;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      Ada.Text_IO.Set_Input (File);
      -- Feeder:=  OpenToken.Text_Feeder.Text_IO.Create (Input_File'Unchecked_Access);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Tokenizer.Find_Next (Analyzer);
--        OpenToken.Token.Parse (Match    => Sentence_List.all,
--                               Analyzer => Analyzer);
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line
           (File_Name & ":" & Integer'Image (Tokenizer.Line (Analyzer)) &
            ":" & Integer'Image (Tokenizer.Column (Analyzer)) &
            ": parse exception");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end;

   Ada.Text_IO.Close (File => File);

end Archicheck.Rules_Parser.Parse;
