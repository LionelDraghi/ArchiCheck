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

   Log_File_Name   : constant String := "testrec.txt";
   State_File_Name : constant String := ".testrec";

end Testrec.Settings;
