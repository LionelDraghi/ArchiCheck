-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.Cmd_Line body

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Archicheck.IO;
with Archicheck.Settings;
with Archicheck.Sources;

package body Archicheck.Cmd_Line is

   use Ada.Strings.Unbounded;

   -- -------------------------------------------------------------------------
   Arg_Counter   : Positive := 1;
   Src_Dir_Given : Boolean := False;

   -- -------------------------------------------------------------------------
   procedure Put_Version is
      use Archicheck.IO;
   begin
      Put_Line ("ArchiCheck version " & Settings.ArchiCheck_Version);
      New_Line;
   end Put_Version;

   -- -------------------------------------------------------------------------
   procedure Process_Directory_Option (Line_OK : out Boolean) is
      use Archicheck.IO;

   begin
      Src_Dir_Given := True;
      if Ada.Command_Line.Argument_Count < Arg_Counter + 1 then
         Put_Error ("Sources directory expected after -I");
         Line_OK := False;
      else
         declare
            use Ada.Command_Line;
            use Ada.Directories;
            Full_Name   : constant String :=
                            Ada.Directories.Full_Name   (Argument (Arg_Counter + 1));
            Simple_Name : constant String :=
                            Ada.Directories.Simple_Name (Argument (Arg_Counter + 1));

         begin
            if Exists (Full_Name) then
               if Kind (Full_Name) = Directory then
                  -- Ada.Text_IO.Put_Line ("Analysing directory " & Name);
                  declare
                     Search : Search_Type;
                     Directory_Entry : Directory_Entry_Type;
                  begin
                     Start_Search
                       (Search    => Search,
                        Directory => Full_Name,
                        Pattern   => "*.ad[asb]", --** hard coded for usual Ada convention
                        Filter    => (Directory => False,
                                      others    => True));
                     while More_Entries (Search) loop
                        Get_Next_Entry (Search, Directory_Entry);
                        Archicheck.Sources.Source_Lists.Append
                          (Sources.Source_List,
                           (Name     => To_Unbounded_String
                                (Ada.Directories.Full_Name (Directory_Entry)),
                            Time_Tag => Modification_Time (Directory_Entry)));
                        -- Put_Debug_Line ("trouvé : " & Full_Name (Directory_Entry));
                     end loop;
                     End_Search (Search);
                     Line_OK := True;
                  end;

               else
                 Put_Error (Simple_Name & " is not a directory");
                 Line_OK := False;
               end if;
            else
               Put_Error ("No " & Simple_Name & " directory");
               Line_OK := False;
            end if;
         end;
         Arg_Counter := Arg_Counter + 2;
      end if;

   end Process_Directory_Option;

   -- Procedure: Options_Coherency_Tests
   -- -------------------------------------------------------------------------
   -- This procedure checks various pathologic situations, for example an option
   -- implying source files, but no -I option is given, or no sourc files found.
   -- -------------------------------------------------------------------------
   procedure Options_Coherency_Tests (Line_OK : in out Boolean) is
      use Archicheck.IO;

   begin
      -- Put_Error ("Settings.Src_Needed  : " & Boolean'Image (Settings.Src_Needed));
      -- Put_Error ("Rules_File_Needed : " & Boolean'Image (Settings.Rules_File_Needed));
      -- Put_Error ("Src List is empty : "
      -- & Boolean'Image (Source_Lists.Is_Empty (Sources.Source_List)));

      -- first, let's eliminate the normal situation :
      -- there is a rules file, and there are sources to analyze
      if Settings.Rules_File_Name = "" or Sources.Source_List.Is_Empty then

         -- Note that those tests are not all mutually exclusive. More specific
         -- case are tested first, to let more general messages at the end.

         if Settings.Rules_File_Name = "" and Settings.List_Rules then
            Put_Error ("No rules file given", With_Help => True);
            Line_OK := False;

         elsif not Sources.Source_List.Is_Empty and not Settings.Src_Needed then
            Put_Error ("Nothing to do with those sources", With_Help => True);
            Line_OK := False;

         elsif Sources.Source_List.Is_Empty and Settings.List_Dependencies then
            Put_Error ("Cannot list dependencies, no sources found", With_Help => True);
            Line_OK := False;

         elsif Sources.Source_List.Is_Empty and Settings.List_Files and Src_Dir_Given then
            Put_Error ("Cannot list files, no sources found to analyze");
            Line_OK := False;

         elsif Sources.Source_List.Is_Empty and Src_Dir_Given then
            Put_Error ("No src found in those directories", With_Help => True);
            Line_OK := False;

         elsif Settings.Rules_File_Name /= "" and not Settings.Rules_File_Needed then
            Put_Error ("Nothing to do with this rules file", With_Help => True);
            Line_OK := False;

         end if;

      end if;
   end Options_Coherency_Tests;

   -- -------------------------------------------------------------------------
   procedure Analyze_Cmd_Line (Line_OK : out Boolean) is
      use Archicheck.IO;

   begin
      Line_OK := True;

      if Ada.Command_Line.Argument_Count < 1 then
         Put_Help;
         return;
      end if;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "-I" then
               Process_Directory_Option (Line_OK);
               if not Line_OK then return; end if;

            elsif Opt = "-lf" or Opt = "--list_files" then
               Settings.List_Files := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-ld" or Opt = "--list_dependencies" then
               Settings.List_Dependencies := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-lr" or Opt = "--list_rules" then
               Settings.List_Rules := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "--version" then
               Put_Version;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-h" or Opt = "--help" then
               Put_Help;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-q" or Opt = "--quiet" then
               Settings.Quiet_Mode := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-v" or Opt = "--verbose" then
               Settings.Verbose_Mode := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-d" then
               -- undocumented option
               Settings.Debug_Mode := True;
               Arg_Counter := Arg_Counter + 1;

               -- elsif Arg_Counter = Ada.Command_Line.Argument_Count then
            elsif Ada.Directories.Exists (Opt) then
               -- should be the rules file
               Settings.Set_Rules_File_Name (Opt);
               Arg_Counter := Arg_Counter + 1;

            else
               Put_Error ("Unknown rules file or unknow option " & Opt, With_Help => True);
               Line_OK := False;

            end if;

            exit when not Line_OK;
         end;

      end loop;

      Options_Coherency_Tests (Line_OK);

   end Analyze_Cmd_Line;

end Archicheck.Cmd_Line;
