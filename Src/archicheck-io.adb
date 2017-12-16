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

with Ada.Strings.Fixed;
-- with Ada.Directories;

package body Archicheck.IO is

   -- -------------------------------------------------------------------------
   -- Procedure: Put_Help
   -- -------------------------------------------------------------------------
   procedure Put_Help is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("ArchiCheck normal use :");
      Ada.Text_IO.Put_Line ("   archicheck rules_file -I directory [-I directory]*");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("General form :");
      Ada.Text_IO.Put_Line ("   archicheck [Options] [rules_file] [-I directory]*");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Options :");
      Ada.Text_IO.Put_Line ("   -lf | --list_files        : list sources files analyzed");
      Ada.Text_IO.Put_Line ("   -ld | --list_dependencies : list identified units and dependencies"
                & " in analyzed sources files");
      Ada.Text_IO.Put_Line ("   -lr | --list_rules        : list rules in a rules file");
      Ada.Text_IO.Put_Line ("   -r  | --recursive         : all following -I are recursive");
      -- Ada.Text_IO.Put_Line ("   NB : when one of the list options above is used, checks are NOT performed");
      --** copyright and disclaimer to be added?
      Ada.Text_IO.Put_Line ("   -v  | --verbose");
      Ada.Text_IO.Put_Line ("   -q  | --quiet             : no message unless error. Warning are also ignored.");
      Ada.Text_IO.Put_Line ("         --version           : archicheck version");
      Ada.Text_IO.Put_Line ("   -h  | --help              : this message");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Examples:");
      Ada.Text_IO.Put_Line ("   archicheck rules.txt -I ./src");
      Ada.Text_IO.Put_Line ("   archicheck -lf -I ./src");
      Ada.Text_IO.Put_Line ("   archicheck -lr rules.txt");
      Ada.Text_IO.New_Line;
   end Put_Help;

   -- -------------------------------------------------------------------------
   -- Procedure: Put_Warning
   -- -------------------------------------------------------------------------
   procedure Put_Warning (Msg : in String := "") is
   begin
      Put_Line ("Warning : " & Msg);
      -- use the local version of Put_Line, and not the Ada.Text_IO one, so that
      -- Warning messages are also ignored when --quiet.
   end Put_Warning;

   -- -------------------------------------------------------------------------
   -- Procedure: Put_Error
   -- -------------------------------------------------------------------------
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is
   begin
      Ada.Text_IO.Put_Line ("Error : " & Msg);
      if With_Help then Put_Help; end if;
   end Put_Error;

   -- -------------------------------------------------------------------------
   -- Procedure: Put_Debug_Line
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (Prefix & " | " & Msg);
      end if;
   end Put_Debug_Line;

   -- --------------------------------------------------------------------------
   -- Procedure: Put_Debug
   -- -------------------------------------------------------------------------
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

   -- -------------------------------------------------------------------------
   -- Procedure: New_Debug_Line
   -- -------------------------------------------------------------------------
   procedure New_Debug_Line (Debug  : in Boolean) is
   begin
      if Debug then
         Ada.Text_IO.New_Line;
      end if;
   end New_Debug_Line;

   -- -------------------------------------------------------------------------
   -- Procedure: Put_Line
   -- -------------------------------------------------------------------------
   procedure Put_Line (Item  : String;
                       Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put_Line (Item);
      end if;
   end Put_Line;

   -- -------------------------------------------------------------------------
   -- Procedure: Put
   -- -------------------------------------------------------------------------
   procedure Put (Item  : String;
                  Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put (Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   -- Procedure: New_Line
   -- -------------------------------------------------------------------------
   procedure New_Line (Spacing           : Ada.Text_IO.Positive_Count := 1;
                       Level             : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.New_Line (Spacing);
      end if;
   end New_Line;

   -- -------------------------------------------------------------------------
   -- Function: GNU_Prefix body
   -- -------------------------------------------------------------------------
   function GNU_Prefix (File   : in String;
                        Line   : in Positive;
                        Column : in Integer := 0) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimed_File   : constant String := Trim (File, Side => Both);
      Trimed_Line   : constant String := Trim (Positive'Image (Line), Side => Both);
      Trimed_Column : constant String := Trim (Integer'Image (Column), Side => Both);
   begin
      if Column = 0 then
         return Trimed_File & ":" & Trimed_Line & ": ";
      else
         return Trimed_File & ":" & Trimed_Line & "." & Trimed_Column & ": ";
      end if;
   end GNU_Prefix;

   --     -- --------------------------------------------------------------------------
   --     -- Function: GNU_Prefix
   --     -- -------------------------------------------------------------------------
   --     function GNU_Prefix (File   : in Ada.Text_IO.File_Type;
   --                          Line   : in Positive;
   --                          Column : in Integer := 0) return String is
   --     begin
   --        return GNU_Prefix (File   => Ada.Directories.Full_Name (Ada.Text_IO.Name (File)),
   --                           Line   => Line,
   --                           Column => Column);
   --     end GNU_Prefix;

end Archicheck.IO;

