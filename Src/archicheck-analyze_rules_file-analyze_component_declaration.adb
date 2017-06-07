-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Archicheck.Analyze_Rules_File.Analyze_Component_Declaration

-- with OpenToken.Text_Feeder.String;
--with Archicheck.Rules_Parser; use Archicheck.Rules_Parser;
-- with OpenToken.Recognizer.Nothing;

separate (Archicheck.Analyze_Rules_File)

procedure Analyze_Component_Declaration (Line : in String) is
--   Declaration.all := Selection.Class (Units_T & In_T & Identifier_T);
--     -- File : "units in Dir1/*.ads"

--     File_List.all := Selection.Class
--       (File or
--        Sequence.New_Instance (File & Selection.Class (And_T or Comma_T)));
--     -- File_List : File [(and|,) File]
--     -- example : units in Dir1/*.ads, units in Dir2 and units in Dir3"

begin
   null;
   --     Component_Name.all := Selection.Class
--       (Identifier_T
--        or Sequence.New_Instance (Minus_T & Identifier_T));
   -- Component_Name = "GUI" | "- GUI"
   -- within "- GUI contains Gtk"

--     Component_Name.all := Sequence.New_Instance
--       (new Selection.Class'Instance (Minus_T or Component_Name));

--     Unit_List.all := Selection.Class
--       (Identifier_T
--        or Sequence.New_Instance (Identifier_T & Comma_T & Identifier_T)
--        or Sequence.New_Instance (Identifier_T & And_T   & Identifier_T));
--
--     Declaration.all := Sequence.New_Instance
--       (Component_Name & Contains_T & new Selection.Instance'
--          (Unit_List or Sequence.New_Instance (Also_T & Unit_List))).all;
--
--     OpenToken.Text_Feeder.String.Set (Feeder, Value => Line);
--     Tokenizer.Find_Next (Analyzer);
--
--     Put_Line ("declaration");
--     OpenToken.Token.Parse (Match    => Declaration.all,
--                            Analyzer => Analyzer);

--  exception
--     when others => --Error : OpenToken.Token.Parse_Error =>
--        Ada.Text_IO.Put_Line
--          (File_Name & ":" & Integer'Image (Tokenizer.Line (Analyzer)) &
--           ":" & Integer'Image (Tokenizer.Column (Analyzer)) &
--           ": parse exception");
--        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));

end Analyze_Component_Declaration;
