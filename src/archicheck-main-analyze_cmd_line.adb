-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Cmd_Line body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Lang;
with Archicheck.Settings;
with Archicheck.Sources;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

separate (Archicheck.Main)

procedure Analyze_Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter    : Positive := 1;
   Src_Dir_Given  : Boolean := False;

   -- --------------------------------------------------------------------------
   -- Procedure: Next_Arg
   -- --------------------------------------------------------------------------
   procedure Next_Arg is
   begin
      Arg_Counter := Arg_Counter + 1;
   end Next_Arg;

   -- --------------------------------------------------------------------------
   -- Procedure: Put_Version
   -- --------------------------------------------------------------------------
   procedure Put_Version is
      use Archicheck.IO;
   begin
      Put_Line (Settings.ArchiCheck_Version);
   end Put_Version;

   -- --------------------------------------------------------------------------
   -- Procedure: Process_Directory_Option
   -- --------------------------------------------------------------------------
   procedure Process_Directory_Option (Recursive : in Boolean) is
   begin
      Src_Dir_Given := True;
      if Ada.Command_Line.Argument_Count < Arg_Counter + 1 then
         Put_Error ("Sources directory expected after -I");

      else
         declare
            use Ada.Command_Line;
            use Ada.Directories;
            Dir_Name : constant String := Argument (Arg_Counter + 1);

         begin
            if Exists (Dir_Name) then
               if Kind (Dir_Name) = Directory then
                  -- let's collect all sources in that dir:
                  Lang.Get_Src_List (Root_Dir  => Dir_Name,
                                     Recursive => Recursive);
               else
                  Put_Error (Dir_Name & " is not a directory");

               end if;

            else
               Put_Error ("No " & Dir_Name & " directory");

            end if;
         end;
         Arg_Counter := Arg_Counter + 2;
      end if;

   end Process_Directory_Option;

   -- --------------------------------------------------------------------------
   -- Procedure: Process_Rule
   -- --------------------------------------------------------------------------
   procedure Process_Rule is
      use Ada.Command_Line;
      use Ada.Text_IO;
   begin
      if Argument_Count < Arg_Counter + 1 then
         Put_Error ("Rule expected after --append_rule");

      else
         if not Is_Open (Settings.Cmd_Line_Rules_File) then
            Create (File => Settings.Cmd_Line_Rules_File, Mode => Out_File);
         end if;
         Put_Line (Settings.Cmd_Line_Rules_File, Argument (Arg_Counter + 1));
         -- Put_Error ("Rule found : " & Argument (Arg_Counter + 1));
         Arg_Counter := Arg_Counter + 2;
      end if;

   end Process_Rule;

   -- --------------------------------------------------------------------------
   -- Procedure: Options_Coherency_Tests
   --
   -- Purpose:
   --    This procedure checks various pathologic situations, for example an
   --    option implying source files, but no -I option is given, or no source
   --    files found.
   -- --------------------------------------------------------------------------
   procedure Options_Coherency_Tests is
      use Archicheck.IO;

   begin
      -- first, let's eliminate the normal situation :
      -- there is a rules file, and there are sources to analyze
      if Settings.Rules_File_Name = "" or Sources.Get_List.Is_Empty then

         -- Note that those tests are not all mutually exclusive. More specific
         -- case are tested first, to let more general messages at the end.

         if (Settings.Rules_File_Name = "" and not Settings.Cmd_Line_Rules) and Settings.List_Rules then
            Put_Error ("No rules file given", With_Help => True);

         elsif not Sources.Get_List.Is_Empty and not Settings.Src_Needed then
            Put_Error ("Nothing to do with those sources", With_Help => True);

         elsif Sources.Get_List.Is_Empty and Settings.List_Dependencies then
            Put_Warning ("Cannot list dependencies, no sources found");

         elsif Sources.Get_List.Is_Empty and
           Settings.List_Files and Src_Dir_Given
         then
            Put_Warning ("Cannot list files, no sources found to analyze");

         elsif Sources.Get_List.Is_Empty and Src_Dir_Given then
            Put_Warning ("No src found in those directories");

         elsif Settings.Rules_File_Name /= "" and not
           Settings.Rules_File_Needed
         then
            Put_Error ("Nothing to do with this rules file", With_Help => True);

         end if;

      end if;
   end Options_Coherency_Tests;

   -- -------------------------------------------------------------------------
   -- Procedure: Analyze_Cmd_Line
   -- -------------------------------------------------------------------------
   use Archicheck.IO;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Help;
      return;
   end if;

   while Arg_Counter <= Ada.Command_Line.Argument_Count loop
      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);
      begin
         if Opt = "-I" then
            Process_Directory_Option (Settings.Recursive);
            if Some_Error then return; end if;

         elsif Opt = "-Ir" then
            Process_Directory_Option (Recursive => True);
            if Some_Error then return; end if;

         elsif Opt = "-ar" or Opt = "--append_rule" then
            Settings.Cmd_Line_Rules := True;
            Process_Rule;
            if Some_Error then return; end if;

         elsif Opt = "-lf" or Opt = "--list_files" then
            Settings.List_Files := True;
            Next_Arg;

         elsif Opt = "-ld" or Opt = "--list_dependencies" then
            Settings.List_Dependencies := True;
            Next_Arg;

         elsif Opt = "-lr" or Opt = "--list_rules" then
            Settings.List_Rules := True;
            Next_Arg;

         elsif Opt = "-lnc" or Opt = "--list_non_covered" then
            Settings.List_Non_Covered := True;
            Next_Arg;

         elsif Opt = "-ct" or Opt = "--create_template" then
            Settings.Create_Template := True;
            Next_Arg;

         elsif Opt = "-r" or Opt = "--recursive" then
            Settings.Recursive := True;
            Next_Arg;

         elsif Opt = "--version" then
            Put_Version;
            Next_Arg;

         elsif Opt = "-h" or Opt = "--help" then
            Put_Help;
            Next_Arg;

         elsif Opt = "-q" or Opt = "--quiet" then
            Settings.Verbosity := Quiet;
            Next_Arg;

         elsif Opt = "-We" or Opt = "--Warnings=error" then
            Settings.Warnings_As_Errors := True;
            Next_Arg;

         elsif Opt = "-v" or Opt = "--verbose" then
            Settings.Verbosity := Verbose;
            Next_Arg;

         elsif Opt = "-d" then
            -- undocumented option
            Settings.Verbosity := Debug;
            Next_Arg;

         elsif Ada.Directories.Exists (Opt) then
            -- should be the rules file
            Settings.Set_Rules_File_Name (Opt);
            Next_Arg;

         else
            IO.Put_Error ("Unknown rules file or unknown option " & Opt);

         end if;

         if Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

   end loop;

   Options_Coherency_Tests;

end Analyze_Cmd_Line;
