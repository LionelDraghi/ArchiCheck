-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

private package Testrec.Settings is

   Verbose : Boolean := False;
   Quiet   : Boolean := False;

   type Output_Formats is (NaturalDocs, Markdown);
   Output_Format : Output_Formats := Markdown;

   function Log_File_Name return String;

   State_File_Name : constant String := ".testrec";

   Version : constant String := "v1.1.0";
   -- History:
   -- v1 : default output format changed to Markdown

end Testrec.Settings;
