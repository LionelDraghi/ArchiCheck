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
   Cmd_Line_OK   : Boolean;

begin
   -- language specific processor pluggin:
   Lang.Initialize;

   IO.Put_Line ("Starting src identification", Level => IO.Verbose);
   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if not Cmd_Line_OK then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- 1 - wanna see the sources? (Source_List is build during command_line analysis)
   -- -----------------------------------------------------------------------
   if Settings.List_Files then
      Sources.Dump_Sources (Sources.Get_List);
   end if;

   -- 2 - let's extract dependencies from sources
   -- -----------------------------------------------------------------------
   IO.Put_Line ("Starting src analyzis", Level => IO.Verbose);
   Lang.Analyze_Dependencies;

   if Settings.List_Dependencies then
      Units.Dump;
   end if;

   if Settings.Rules_File_Name /= "" then

      -- 3 - Rules file analyzis
      -- --------------------------------------------------------------------
      IO.Put_Line ("Reading rules file...", Level => IO.Verbose);
      Rules.Parser.Parse (Settings.Rules_File_Name);

      -- 4 - let's run the checks
      -- --------------------------------------------------------------------
      IO.Put_Line ("Checking rules...", Level => IO.Verbose);
      Rules.Check;

   else
      IO.Put_Line ("No rules file provided", Level => IO.Verbose);

   end if;

   if IO.Error_Count /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

end Archicheck.Main;
