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
--   This package manages global settings, hard coded or from cmd line.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

private package Archicheck.Settings is

   ArchiCheck_Version : constant String := "0.5.8";

   List_Files         : Boolean := False;
   List_Dependencies  : Boolean := False;
   List_Rules         : Boolean := False;
   List_Non_Covered   : Boolean := False;
   Recursive          : Boolean := False;
   Warnings_As_Errors : Boolean := False;
   Create_Template    : Boolean := False;

   Template_Name : constant String := "template.ac";

   -- -------------------------------------------------------------------------
   type Print_Out_Level is (Debug, Verbose, Normal, Quiet);
   -- default: Normal messages are displayed, verbose messages are not displayed.
   -- quiet:   Neither normal messages nor verbose messages are displayed.
   --          This mode can be achieved using option --quiet.
   -- verbose: Both normal messages and verbose messages are displayed.
   --          This mode can be achieved using option --verbose.
   Verbosity : Print_Out_Level := Normal;

   -- -------------------------------------------------------------------------
   -- Debug_Mode
   -- -------------------------------------------------------------------------
   function Debug_Mode return Boolean is (Verbosity = Debug);

   -- -------------------------------------------------------------------------
   Ada_Files_Pattern  : constant String := "*.ad[asb]";
   Java_Files_Pattern : constant String := "*.java";
   -- Not sure that case independance would be usefull here

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
