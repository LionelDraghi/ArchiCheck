-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Archicheck.Analyze_Rules_File body

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
with Archicheck.IO;


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
            IO.Put_Error ("Quezako : >" & Line & "<"); --**
            exit Analysis;

         end if;

      end;

   end loop Analysis;

   Close (Rules_File);

end Archicheck.Analyze_Rules_File;
