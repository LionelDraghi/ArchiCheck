-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.IO specification
--
-- Purpose:
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Archicheck.Settings;

with Ada.Text_IO;

private package Archicheck.IO is

   -- --------------------------------------------------------------------------
   procedure Put_Help;

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String);
   procedure Put_Debug (Msg    : in String := "";
                        Debug  : in Boolean;
                        Prefix : in String);
   procedure New_Debug_Line (Debug  : in Boolean);

   -- --------------------------------------------------------------------------
   use type Settings.Print_Out_Level;
   subtype Print_Out_Level is Settings.Print_Out_Level;
   Debug   : constant Print_Out_Level := Settings.Debug;
   Verbose : constant Print_Out_Level := Settings.Verbose;
   Normal  : constant Print_Out_Level := Settings.Normal;
   Quiet   : constant Print_Out_Level := Settings.Quiet;

   -- --------------------------------------------------------------------------
   -- Mimics eponym Text_IO functions, except that :
   --   - if --quiet is set on command line, they have no effect,
   --     unless Even_In_Quiet_Mode is set.
   --   - if Only_When_Verbose is False, they have no effect
   --     unless --verbose is set on command line
   procedure Put_Line (Item  : String;
                       Level : Print_Out_Level := Normal);
   procedure Put      (Item  : String;
                       Level : Print_Out_Level := Normal);
   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1;
                       Level   : Print_Out_Level := Normal);

   -- --------------------------------------------------------------------------
   procedure Put_Warning   (Msg       : in String := "");
   procedure Put_Error     (Msg       : in String := "";
                            With_Help : in Boolean := False);
   procedure Put_Exception (Msg       : in String := "");

   -- --------------------------------------------------------------------------
   -- Error_Count and Warning_Count return the number of call to Put_Error
   -- and Put_Warning.
   function Error_Count   return Natural;
   function Warning_Count return Natural;

   -- Some_Error return True if some error occured, or if some Warning
   -- occured and option to treat warning as error is set.
   function Some_Error return Boolean is
     (Error_Count /= 0 or
        (Settings.Warnings_As_Errors and Warning_Count /= 0));

end Archicheck.IO;
