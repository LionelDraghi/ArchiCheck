-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck body

with Archicheck.Settings;

package body Archicheck.IO is

   -- -------------------------------------------------------------------------
   procedure Put_Help is
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line ("ArchiCheck normal use :");
      Put_Line ("   archicheck rules_file -I directory [-I directory]*");
      New_Line;
      Put_Line ("General form :");
      Put_Line ("   archicheck [Options] [rules_file] [-I directory]*");
      New_Line;
      Put_Line ("Options :");
      Put_Line ("   -lf | --list_files        : list sources files analyzed");
      Put_Line ("   -ld | --list_dependencies : list identified dependencies"
                & " in analyzed sources files");
      Put_Line ("   -lc | --list_components   : list components described in"
                & " a rules file");
      Put_Line ("         --version           : archicheck version");
      --** copyright and disclaimer to be added?
      Put_Line ("   -q  | --quiet             : no message unless error. Warning are also ignored.");
      Put_Line ("   -v  | --verbose");
      Put_Line ("   -h  | --help              : this message");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("   archicheck rules.txt -I ./src");
      Put_Line ("   archicheck -lf -I ./src");
      Put_Line ("   archicheck -lc rules.txt");
      New_Line;
   end Put_Help;

   -- -------------------------------------------------------------------------
   procedure Put_Warning (Msg : in String := "") is
   begin
      Put_Line ("Warning : " & Msg);
      -- use the local version of Put_Line, and not the Ada.Text_IO one, so that
      -- Warning messages are also ignored when --quiet.
   end Put_Warning;

   -- -------------------------------------------------------------------------
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is
   begin
      Ada.Text_IO.Put_Line ("Error : " & Msg);
      if With_Help then Put_Help; end if;
   end Put_Error;

   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (Prefix & " | " & Msg);
      end if;
   end Put_Debug_Line;

   -- -------------------------------------------------------------------------
   procedure Put_Debug (Msg    : in String := "";
                        Debug  : in Boolean;
                        Prefix : in String) is
   begin
      if Debug then
         Ada.Text_IO.Put (Prefix & " | " & Msg);
      end if;
   end Put_Debug;

   -- -------------------------------------------------------------------------
   procedure Put_Line (Item : String) is
   begin
      if not Settings.Quiet_Mode then
         Ada.Text_IO.Put_Line (Item);
      end if;
   end Put_Line;

   procedure Put (Item : String) is
   begin
      if not Settings.Quiet_Mode then
         Ada.Text_IO.Put (Item);
      end if;
   end Put;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      if not Settings.Quiet_Mode then
         Ada.Text_IO.New_Line (Spacing);
      end if;
   end New_Line;

end Archicheck.IO;

