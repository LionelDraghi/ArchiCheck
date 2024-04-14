-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

separate (Archicheck.Main)

procedure Create_Template is
   Template  : File_Type;
   File_Name : String renames Settings.Template_Name;

begin
   if Ada.Directories.Exists (File_Name) then
      Put_Error ("File " & File_Name & " already exists");

   else
      Create (Template, Name => Settings.Template_Name);
      Set_Output (Template);

      Put_Line ("-- ------------------------");
      Put_Line ("-- Template ArchiCheck file");
      Put_Line ("-- ------------------------");
      New_Line;
      Put_Line ("-- ArchiCheck files contain :");
      Put_Line ("-- 1. Comments, prefixed by '--', '\\' or '#'");
      New_Line;
      Put_Line ("-- 2. Component definitions, like: ");
      Put_Line ("Application_Layer contains pkg_1, pkg_2, pkg_3");
      Put_Line ("--    Application_Layer is the component name.");
      Put_Line ("--    It contains compilation units (pkg_1, etc.), or other");
      Put_Line ("--    components, meaning that you can define nested components.");
      New_Line;
      Put_Line ("-- 3. Rules on units and components");
      Put_Line ("Layer_A is a layer over Layer_B            -- Layer declaration");
      Put_Line ("Pango may use Cairo                        -- Use declaration");
      Put_Line ("Only Layer_B may use Interfaces.C          -- Restricted use declaration");
      Put_Line ("Ada.Containers.Indefinite use is forbidden -- Forbidden use");
      Put_Line ("Java.IO use is allowed                     -- Allowed use");
      New_Line;
      Put_Line ("--    Note that wildcard are not yet implemented, but");
      Put_Line ("--    Java.IO means Java.IO and Java.IO.*");
      New_Line;
      Put_Line ("-- More extensive explanations : http://lionel.draghi.free.fr/Archicheck/rules/");
      Put_Line ("-- ");
      Put_Line ("-- File generated with ArchiCheck " & Settings.ArchiCheck_Version);

      Close (Template);
   end if;

end Create_Template;
