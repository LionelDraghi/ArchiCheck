-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.Settings specification

private package Archicheck.Settings is

   ArchiCheck_Version : constant String := "0.2";

   List_Files        : Boolean := False;
   List_Dependencies : Boolean := False;
   List_Rules        : Boolean := False;
   Quiet_Mode        : Boolean := False;
   Verbose_Mode      : Boolean := False;

   -- Undocumented option :
   Debug_Mode : Boolean := False;

   -- -------------------------------------------------------------------------
   procedure Set_Rules_File_Name (Name : in String);
   function Rules_File_Name return String;

   -- -------------------------------------------------------------------------
   -- Function: Src_Needed
   -- return True if some of the analyzed _at this stage_ options are dependent of -I options
   function Src_Needed return Boolean;

   -- -------------------------------------------------------------------------
   -- Function: Rules_File_Needed
   -- return True if some of the analyzed _at this stage_ options are dependent of the rules file
   function Rules_File_Needed return Boolean;

end Archicheck.Settings;
