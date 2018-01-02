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
with Archicheck.Rules.Parser;
with Archicheck.Rules.Check;
with Archicheck.Settings;
with Archicheck.Sources;
with Archicheck.Units;

with Ada.Command_Line;

procedure Archicheck.Main is

begin
   -- language specific processor pluggin:
   Lang.Initialize;

   IO.Put_Line ("1. Starting src identification", Level => IO.Verbose);
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

   IO.Put_Line ("2. Starting src analyzis", Level => IO.Verbose);
   Lang.Analyze_Dependencies;

   if Settings.List_Dependencies then
      Units.Dump;
      return;
   end if;

   if Settings.Rules_File_Name /= "" then

      IO.Put_Line ("3. Reading rules file...", Level => IO.Verbose);
      Rules.Parser.Parse (Settings.Rules_File_Name);

      if IO.Some_Error then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;

      else
         -- --------------------------------------------------------------------
         IO.Put_Line ("4. Checking rules...", Level => IO.Verbose);
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
