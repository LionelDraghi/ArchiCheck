-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Archicheck.Main body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Archicheck.Cmd_Line;
with Archicheck.IO;
with Archicheck.Lang;
with Archicheck.Lang.Initialize;
with Archicheck.Rules.Check;
with Archicheck.Rules.Parser;
with Archicheck.Rules.Print_Non_Covered_Unit;
with Archicheck.Settings;
with Archicheck.Sources;         use Archicheck.Sources;
with Archicheck.Units;

with Ada.Command_Line;

procedure Archicheck.Main is

begin
   -- language specific processor pluggin:
   Lang.Initialize;

   -- 1. Starting src identification
   Cmd_Line.Analyze_Cmd_Line;

   if IO.Some_Error then
      -- Some error occurs during command line analisys, stop here.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- Wanna see the sources? (Source_List is build during command_line analysis)
   -- -----------------------------------------------------------------------
   if Settings.List_Files then
      Sources.Dump_Sources (Sources.Get_List);
      return;
   end if;

   -- 2. Starting src analyzis
   Lang.Analyze_Dependencies;

   if Settings.List_Dependencies then
      Units.Dump;
      return;
   end if;

   if Settings.Rules_File_Name /= "" then

      -- 3. Reading rules file
      Rules.Parser.Parse (+Settings.Rules_File_Name);

      if IO.Some_Error then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;

      elsif Settings.List_Rules then
         -- This has been done during rules parsing, so we can quit.
         --
         -- For coherency purpose in debug and verbose messages
         -- Rules are listed on the fly, as soon as detected during
         -- the rules file parsing.
         -- (This is why there is no Rules.Dump procedure).
         --
         return;

      elsif Settings.List_Non_Covered then
         Rules.Print_Non_Covered;

      else
         -- 4. Checking rules
         Rules.Check;

      end if;

   else
      IO.Put_Line ("No rules file provided", Level => IO.Verbose);

   end if;

   if IO.Some_Error then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

end Archicheck.Main;
