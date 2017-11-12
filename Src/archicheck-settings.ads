-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Settings specification
--
-- Purpose:
--   This package defines Source, manage the Source list, and provides utilities
--   to print this list.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

private package Archicheck.Settings is

   ArchiCheck_Version : constant String := "0.4.0";

   List_Files        : Boolean := False;
   List_Dependencies : Boolean := False;
   List_Rules        : Boolean := False;
   Quiet_Mode        : Boolean := False;
   Verbose_Mode      : Boolean := False;

   -- -------------------------------------------------------------------------
   Ada_Files_Pattern  : constant String := "*.[aA][dD][asbASB]";
   Java_Files_Pattern : constant String := "*.[jJ][aA][vV][aA]";
   -- Not sure that this case independance mess is usefull

   -- Undocumented option :
   Debug_Mode : Boolean := False;

   -- -------------------------------------------------------------------------
   -- Procedure: Set_Rules_File_Name
   -- -------------------------------------------------------------------------
   procedure Set_Rules_File_Name (Name : in String);

   -- -------------------------------------------------------------------------
   -- Function: Rules_File_Name
   -- -------------------------------------------------------------------------
   function Rules_File_Name return String;

   -- -------------------------------------------------------------------------
   -- Function: Src_Needed
   --
   -- Purpose:
   --   return True if some of the analyzed _at this stage_ options are
   --   dependent of -I options
   -- -------------------------------------------------------------------------
   function Src_Needed return Boolean;

   -- -------------------------------------------------------------------------
   -- Function: Rules_File_Needed
   --
   -- Purpose:
   --    return True if some of the analyzed _at this stage_ options are
   --    dependent of the rules file
   -- -------------------------------------------------------------------------
   function Rules_File_Needed return Boolean;

end Archicheck.Settings;
