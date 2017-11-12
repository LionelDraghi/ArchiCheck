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
with Archicheck.Settings;
with Archicheck.Rules_Parser;
with Archicheck.Check_Layer_Rules;
with Archicheck.Sources;
with Archicheck.Lang;
with Archicheck.Lang.Initialize;
with Archicheck.Dependencies;

with Ada.Command_Line;

procedure Archicheck.Main is
   Cmd_Line_OK   : Boolean;

begin
   -- language specific processor pluggin:
   Archicheck.Lang.Initialize;

   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if Cmd_Line_OK then

      -- 1 - wanna see the sources? (Source_List is build during command_line analysis)
      -- -----------------------------------------------------------------------
      if Settings.List_Files then
         Sources.Dump_Sources (Sources.Get_List);
      end if;

      -- 2 - let's extract dependencies from sources
      -- -----------------------------------------------------------------------
--        for Src of Sources.Get_List loop
--           Dependencies.Add_Dependencies (From_Source => Src.Name);
--        end loop;
      Lang.Analyze_Dependencies;

      if Settings.List_Dependencies then
         Dependencies.Dump;
      end if;

      -- 3 - Rules file analyzis
      -- -----------------------------------------------------------------------
      if Settings.Rules_File_Name /= "" then
         Rules_Parser.Parse (Settings.Rules_File_Name);
      end if;

      -- 4 - let's run the checks
      -- -----------------------------------------------------------------------
      Check_Layer_Rules;

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   end if;

end Archicheck.Main;
