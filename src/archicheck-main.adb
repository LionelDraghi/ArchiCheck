-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
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

with Archicheck.IO;
with Archicheck.Lang;
with Archicheck.Lang.Initialize;
with Archicheck.Rules.Check;
with Archicheck.Rules.Parser;
with Archicheck.Rules.Check_Unrelated_Rules_Units;
with Archicheck.Rules.Dump_Unrelated_Compilation_Units;
with Archicheck.Settings;
with Archicheck.Sources;                      use Archicheck.Sources;
with Archicheck.Units;

with Ada.Command_Line;

procedure Archicheck.Main is
   procedure Put_Help         is separate;
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is separate;
   procedure Create_Template  is separate;
   procedure Analyze_Cmd_Line is separate;

begin
   -- language specific processor pluggin:
   Lang.Initialize;

   -- 1. Starting src identification
   Analyze_Cmd_Line;

   if IO.Some_Error then
      -- Some error occurs during command line analysis, stop here.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- 2. Create template rules file
   if Settings.Create_Template then
      Main.Create_Template;
      return;
   end if;

   -- Wanna see the sources? (Source_List is build during command_line analysis)
   -- -----------------------------------------------------------------------
   if Settings.List_Files then
      Sources.Sort_And_Dump_Sources;
      return;
   end if;

   -- 3. Starting src analyzis
   Lang.Analyze_Dependencies;

   if Settings.List_Dependencies then
      Units.Dump;
      return;
   end if;

   if Settings.Rules_File_Name /= "" then

      -- 4. Reading rules file
      Rules.Parser.Parse (+Settings.Rules_File_Name);
      if not Sources.Get_List.Is_Empty then
         -- We ensure that some sources where found, so that
         -- to avoid useless warning on non matching sources
         -- if the user just entended to check the rules file.
         Rules.Check_Unrelated_Rules_Units;
      end if;

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
         -- 5. Checking compilation units not involved in rules
         Rules.Dump_Unrelated_Compilation_Units;

      else
         -- 6. Checking rules
         Rules.Check;

      end if;

   else
      IO.Put_Line ("No rules file provided", Level => IO.Verbose);

   end if;

   if IO.Some_Error then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

end Archicheck.Main;
