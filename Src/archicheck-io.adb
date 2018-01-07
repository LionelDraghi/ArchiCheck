-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.IO body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

package body Archicheck.IO is

   -- --------------------------------------------------------------------------
   procedure Put_Help is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("ArchiCheck normal use :");
      Ada.Text_IO.Put_Line ("   archicheck rules_file -r -I directory [-I directory]*");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("General form :");
      Ada.Text_IO.Put_Line ("   archicheck [Queries] [rules_file] [Options]* [-I directory]*");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Options :");
      Ada.Text_IO.Put_Line ("   -r  | --recursive      : all following -I are recursive");
      Ada.Text_IO.Put_Line ("   -We | --Warnings=error : Treat warnings as errors");
      Ada.Text_IO.Put_Line ("   -v  | --verbose");
      Ada.Text_IO.Put_Line ("   -q  | --quiet          : no message unless error. Warning are also ignored.");
      Ada.Text_IO.Put_Line ("         --version        : archicheck version");
      Ada.Text_IO.Put_Line ("   -h  | --help           : this message");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Queries :");
      Ada.Text_IO.Put_Line ("   -lf | --list_files        : list analyzed sources files");
      Ada.Text_IO.Put_Line ("   -ld | --list_dependencies : list identified units and dependencies"
                            & " in analyzed sources files");
      Ada.Text_IO.Put_Line ("   -lr | --list_rules        : list rules in a rules file");
      Ada.Text_IO.Put_Line ("   If any, only one of the queries is performed,");
      Ada.Text_IO.Put_Line ("   and then the full analisys on sources is not done.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Examples:");
      Ada.Text_IO.Put_Line ("   archicheck rules.txt -I ./src");
      Ada.Text_IO.Put_Line ("   archicheck -lf -I ./src");
      Ada.Text_IO.Put_Line ("   archicheck -lr rules.txt");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("http://lionel.draghi.free.fr/Archicheck/index.html");
      Ada.Text_IO.New_Line;
   end Put_Help;

   Warnings : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Warning (Msg : in String := "") is
   begin
      Warnings := Warnings + 1;
      Put_Line ("Warning : " & Msg);
      -- use the local version of Put_Line, and not the Ada.Text_IO one,
      -- so that Warning messages are also ignored when --quiet.
   end Put_Warning;

   Errors : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is
   begin
      Errors := Errors + 1;
      Put_Line ("Error : " & Msg, Level => Quiet);
      -- Error Msg should not be ignored
      if With_Help then Put_Help; end if;
   end Put_Error;

   -- --------------------------------------------------------------------------
   procedure Put_Exception (Msg : in String := "") is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Warning : " & Msg);
   end Put_Exception;

   -- --------------------------------------------------------------------------
   function Error_Count   return Natural is (Errors);
   function Warning_Count return Natural is (Warnings);

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (Prefix & " | " & Msg);
      end if;
   end Put_Debug_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Debug (Msg    : in String := "";
                        Debug  : in Boolean;
                        Prefix : in String) is
   begin
      if Debug then
         if Prefix = "" then
            Ada.Text_IO.Put (Msg);
         else
            Ada.Text_IO.Put (Prefix & " | " & Msg);
         end if;
      end if;
   end Put_Debug;

   -- --------------------------------------------------------------------------
   procedure New_Debug_Line (Debug  : in Boolean) is
   begin
      if Debug then
         Ada.Text_IO.New_Line;
      end if;
   end New_Debug_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item  : String;
                       Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put_Line (Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put (Item  : String;
                  Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put (Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1;
                       Level   : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.New_Line (Spacing);
      end if;
   end New_Line;

end Archicheck.IO;

