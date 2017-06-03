-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Archicheck.Analyze_Rules_File body

-- with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;

-- with OpenToken.Token.Sequence; use OpenToken.Token;
-- with OpenToken.Token.Selection;

-- use type OpenToken.Token.Sequence.Instance;
-- use type OpenToken.Token.Selection.Instance;

--with Archicheck.Rules_Lexer; use Archicheck.Rules_Lexer;
-- with OpenToken.Text_Feeder;
-- with OpenToken.Text_Feeder.Text_IO;

procedure Archicheck.Analyze_Rules_File (File_Name  : in     String;
                                         Components : in out Component_Maps.Map)
                                         --OK         :    out Boolean)
is
   pragma Unreferenced (Components);

   Rules_File : Ada.Text_IO.File_Type;

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   function Is_Empty
     (Line : in String) return Boolean is separate;
   function Is_A_Comment
     (Line : in String) return Boolean is separate;
   function Is_A_Component_Declaration
     (Line : in String) return Boolean is separate;
   function Is_A_Layer_Rule
     (Line : in String) return Boolean is separate;
   function Is_A_Dependency_Rule
     (Line : in String) return Boolean is separate;
   function Is_A_Use_Restriction_Rule
     (Line : in String) return Boolean is separate;
   procedure Analyze_Component_Declaration
     (Line : in String) is separate;

begin
   Open (Rules_File, Mode => In_File, Name => File_Name);

   Analysis : while not End_Of_File (Rules_File) loop
      declare
         Line : constant String
           := Trim (Get_Line (Rules_File), Side => Both);
      begin
         -- Put_Line ("Line : >" & Line & "<"); --**
         if Is_A_Comment (Line) then
            null;

         elsif Is_Empty (Line) then
            null;

         elsif Is_A_Component_Declaration (Line) then
            Analyze_Component_Declaration (Line);

         elsif Is_A_Layer_Rule (Line) then
            null;

         elsif Is_A_Dependency_Rule (Line) then
            null;

         elsif Is_A_Use_Restriction_Rule (Line) then
            null;

         else
            -- OK := False;
            Put_Line ("Quezako : >" & Line & "<"); --**
            exit Analysis;

         end if;

      end;

   end loop Analysis;

   Close (Rules_File);
--
--              Sentence.all  := Selection.Class (Rule
--                                       or Comment_T
--                                       or EoL_T
--                                       or Declaration);

--   Declaration.all := Selection.Class (Units_T & In_T & Identifier_T);
--     -- File : "units in Dir1/*.ads"

--     File_List.all := Selection.Class
--       (File or
--        Sequence.New_Instance (File & Selection.Class (And_T or Comma_T)));
--     -- File_List : File [(and|,) File]
--     -- example : units in Dir1/*.ads, units in Dir2 and units in Dir3"

--     Unit_List.all := Selection.Class
--       (Identifier_T
--        or Sequence.New_Instance (Identifier_T & Comma_T & Identifier_T)
--        or Sequence.New_Instance (Identifier_T & And_T   & Identifier_T));
--     Declaration.all  := Selection.Class
--       (Sequence.New_Instance
--          (Identifier_T & Contains_T & Unit_List & EoL_T) or EoF_T);
--     Rule.all         := Selection.Class
--       (Sequence.New_Instance
--          (Identifier_T & Is_T & A_T & Layer_T &
--           Over_T & Identifier_T & EoL_T) or EoF_T);
--  --     Comment_Line.all := Selection.Class
--  --       (Sequence.New_Instance (Comment_T & Identifier_T & EoL_T));

--     Ada.Text_IO.Open (File => Rules_File,
--                       Mode => Ada.Text_IO.In_File,
--                       Name => File_Name);
--     Ada.Text_IO.Set_Input (Rules_File);
--     Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;
--
--     begin
--        Source_Analysis : loop
--           Tokenizer.Find_Next (Analyzer);
--  --           if OpenToken.Token.Parse (Match    => Sentence.all,
--  --                                     Analyzer => Analyzer)
--  --           then
--              Put_Line ("declaration");
--              Rule_Token.Parse (Match    => Sentence.all,
--                                     Analyzer => Analyzer);
--  --           elsif OpenToken.Token.Could_Parse_To (Match    => Rule.all,
--  --                                                 Analyzer => Analyzer)
--  --           then
--  --              Put_Line ("rule");
--  --              OpenToken.Token.Parse (Match    => Rule.all,
--  --                                     Analyzer => Analyzer);
--  --           elsif OpenToken.Token.Could_Parse_To (Match    => Comment_Line.all,
--  --                                                 Analyzer => Analyzer)
--  --           then
--  --              Put_Line ("comment");
--  --              OpenToken.Token.Parse (Match    => Comment_Line.all,
--  --                                     Analyzer => Analyzer);
--  --           end if;
--           exit Source_Analysis when Tokenizer.ID (Analyzer) = EoF_Id;
--        end loop Source_Analysis;

--     exception
--        when Error : OpenToken.Parse_Error =>
--           Ada.Text_IO.Put_Line
--             (File_Name & ":" & Integer'Image (Tokenizer.Line (Analyzer)) &
--              ":" & Integer'Image (Tokenizer.Column (Analyzer)) &
--              ": parse exception");
--           Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
--     end;

end Archicheck.Analyze_Rules_File;
