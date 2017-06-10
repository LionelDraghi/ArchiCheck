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
with Ada.Text_IO;

package body Archicheck.Cmd_Line is

   use Ada.Strings.Unbounded;

   -- -------------------------------------------------------------------------
   Arg_Counter   : Positive := 1;
   Src_Dir_Given : Boolean := False;

   -- -------------------------------------------------------------------------
   package Local is
      Source_List       : Archicheck.Source_Lists.List;
      List_Files        : Boolean := False;
      List_Dependencies : Boolean := False;
      List_Components   : Boolean := False;
      Rules_File_Name   : Unbounded_String;
      -- Function: Src_Needed
      -- return True if some of the analyzed _at this stage_ options are dependent of -I options
      function Src_Needed return Boolean;
      -- Function: Rules_File_Needed
      -- return True if some of the analyzed _at this stage_ options are dependent of the rules file
      function Rules_File_Needed return Boolean;
   end Local;

   -- -------------------------------------------------------------------------
   package body Local is
      function Src_Needed return Boolean is
      begin
         return List_Files or List_Dependencies;
      end Src_Needed;
      function Rules_File_Needed return Boolean is
      begin
         return List_Components;
      end Rules_File_Needed;
   end Local;

   -- -------------------------------------------------------------------------
   procedure Put_Version is
      use Ada.Text_IO;
   begin
      Put_Line ("ArchiCheck version " & ArchiCheck_Version);
      New_Line;
   end Put_Version;

   -- -------------------------------------------------------------------------
   procedure Put_Help is
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line ("ArchiCheck  normal use :");
      Put_Line ("   archicheck rules_file -I directory [-I directory]*");
      New_Line;
      Put_Line ("General form :");
      Put_Line ("   archicheck [Options] [rules_file] [-I directory]*");
      New_Line;
      Put_Line ("Options :");
      Put_Line ("   -lf | --list_files        : list sources files analyzed");
      Put_Line ("   -ld | --list_dependencies : list identified dependencies in analyzed sources files");
      Put_Line ("   -lc | --list_components   : list components described in a rules file");
      Put_Line ("   -v  | --version           : archicheck version"); --version, copyright and disclaimer
      Put_Line ("   -h  | --help              : this message");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("   archicheck rules.txt -I ./src");
      Put_Line ("   archicheck -lf -I ./src");
      Put_Line ("   archicheck -lc rules.txt");
      New_Line;
   end Put_Help;

   -- -------------------------------------------------------------------------
   procedure Put_Err_Msg (Msg       : String;
                          With_Help : Boolean := False) is
     use Ada.Text_IO;
   begin
      Put_Line ("** " & Msg);
      if With_Help then Put_Help; end if;
   end Put_Err_Msg;

     -- -------------------------------------------------------------------------
   procedure Process_Directory_Option (Line_OK : out Boolean) is
   begin
      Src_Dir_Given := True;
      if Ada.Command_Line.Argument_Count < Arg_Counter + 1 then
         Put_Err_Msg ("Sources directory expected after -I");
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
                        Pattern   => "*.ad[sb]", --** hard coded for usual Ada convention
                        Filter    => (Directory => False,
                                      others    => True));
                     while More_Entries (Search) loop
                        Get_Next_Entry (Search, Directory_Entry);
                        Archicheck.Source_Lists.Append
                          (Local.Source_List,
                           (Name     => To_Unbounded_String
                                (Ada.Directories.Full_Name (Directory_Entry)),
                            Time_Tag => Modification_Time (Directory_Entry)));
                        -- Ada.Text_IO.Put_Line ("trouvé : " & Full_Name (Directory_Entry));
                     end loop;
                     End_Search (Search);
                     Line_OK := True;
                  end;

               else
                 Put_Err_Msg (Simple_Name & " is not a directory");
                 Line_OK := False;
               end if;
            else
               Put_Err_Msg ("No " & Simple_Name & " directory");
               Line_OK := False;
            end if;
         end;
         Arg_Counter := Arg_Counter + 2;
      end if;

   end Process_Directory_Option;


   -- -------------------------------------------------------------------------
   -- Procedure Options_Coherency_Tests:
   -- This procedure checks various pathologic situations, for example an option
   -- implying source files, but no -I option is given, or no sourc files found.

   procedure Options_Coherency_Tests (Line_OK : in out Boolean) is
   begin

      --           Put_Err_Msg ("Local.Src_Needed  : " & Boolean'Image (Local.Src_Needed));
      --           Put_Err_Msg ("Rules_File_Needed : " & Boolean'Image (Local.Rules_File_Needed));
      --           Put_Err_Msg ("Src List is empty : " & Boolean'Image (Source_Lists.Is_Empty (Local.Source_List)));

      -- first, let's eliminate the normal situation : there is a rules file, and there are sources to analyze
      if Local.Rules_File_Name = "" or Local.Source_List.Is_Empty then

         -- Note that those tests are not all mutually exclusive. More specific
         -- case are tested first, to let more general messages at the end.

         if Local.Rules_File_Name = "" and Local.List_Components then
            Put_Err_Msg ("Cannot list components, no rules file given", With_Help => True);
            Line_OK := False;

         elsif Local.Source_List.Is_Empty and Local.List_Dependencies then
            Put_Err_Msg ("Cannot list dependencies, no sources found", With_Help => True);
            Line_OK := False;

         elsif Local.Source_List.Is_Empty and Local.List_Files and Src_Dir_Given then
            Put_Err_Msg ("Cannot list files, no sources found to analyze");
            Line_OK := False;

         elsif Local.Source_List.Is_Empty and Src_Dir_Given then
            Put_Err_Msg ("No src found in those directories", With_Help => True);
            Line_OK := False;

         elsif not Local.Source_List.Is_Empty and not Local.Src_Needed then
            Put_Err_Msg ("Nothing to do with those sources", With_Help => True);
            Line_OK := False;

         elsif Local.Rules_File_Name /= "" and not Local.Rules_File_Needed then
            Put_Err_Msg ("Nothing to do with this rules file", With_Help => True);
            Line_OK := False;


         end if;

      end if;
   end Options_Coherency_Tests;

   -- -------------------------------------------------------------------------
   procedure Analyze_Cmd_Line (Line_OK : out Boolean) is
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
               Local.List_Files := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-ld" or Opt = "--list_dependencies" then
               Local.List_Dependencies := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-lc" or Opt = "--list_components" then
               Local.List_Components := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-v" or Opt = "--version" then
               Put_Version;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "-h" or Opt = "--help" then
               Put_Help;
               Arg_Counter := Arg_Counter + 1;

               -- elsif Arg_Counter = Ada.Command_Line.Argument_Count then
            elsif Ada.Directories.Exists (Opt) then
               -- should be the rules file
               Local.Rules_File_Name := To_Unbounded_String (Opt);
               Arg_Counter := Arg_Counter + 1;

            else
               Put_Err_Msg ("Unknown rules file or unknow option " & Opt);
               Put_Help;
               Line_OK := False;

            end if;

            exit when not Line_OK;
         end;

      end loop;

      Options_Coherency_Tests (Line_OK);

   end Analyze_Cmd_Line;

   -- -------------------------------------------------------------------------
   function Source_List return Source_Lists.List is
   begin
      return Local.Source_List;
   end Source_List;

   -- -------------------------------------------------------------------------
   function Rules_File_Name  return String is
   begin
      return To_String (Local.Rules_File_Name);
   end Rules_File_Name;

   -- -------------------------------------------------------------------------
   function List_Files return Boolean is
   begin
      return Local.List_Files;
   end List_Files;

   -- -------------------------------------------------------------------------
   function List_Dependencies return Boolean is
   begin
      return Local.List_Dependencies;
   end List_Dependencies;

   -- -------------------------------------------------------------------------
   function List_Components return Boolean is
   begin
      return Local.List_Components;
   end List_Components;

end Archicheck.Cmd_Line;
