-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Testrec_Config;

with Ada.Text_IO;

private package Testrec.Settings is

   Verbose : Boolean := False;
   Quiet   : Boolean := False;

   type Output_Formats is (NaturalDocs, Markdown); --, Standard_Output);
   Output_Format : Output_Formats := Markdown;

   function Default_Log_File_Name return String;
   Log_File : Ada.Text_IO.File_Type;

   State_File_Name : constant String := ".testrec";

   Version : constant String := Testrec_Config.Crate_Version;
   -- History:
   -- v1 : default output format changed to Markdown

end Testrec.Settings;
