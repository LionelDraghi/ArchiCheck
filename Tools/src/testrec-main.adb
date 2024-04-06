-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Testrec.Main
--
-- Testrec Main is mainly in charge of command line analysis.
-- Real job is done in Testrec child packages.
--

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Testrec.Settings;
with Testrec.Current_State;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Directories;


procedure Testrec.Main is

   -- --------------------------------------------------------------------------
   Log_File    : Ada.Text_IO.File_Type;
   Arg_Counter : Positive := 1;

   -- --------------------------------------------------------------------------
   function Empty_Tag      return String is
   begin
      case Settings.Output_Format is
         when Settings.NaturalDocs => return "<Empty>";
         when Settings.Markdown    => return "[Empty](tests_status.md#empty)";
            -- Fixme: hard coded reference to a tests_status.md file
      end case;
   end Empty_Tag;
   -- --------------------------------------------------------------------------
   function Fail_Tag       return String is
   begin
      case Settings.Output_Format is
         when Settings.NaturalDocs => return "<Failed>";
         when Settings.Markdown    => return "[Failed](tests_status.md#failed)";
      end case;
   end Fail_Tag;
   -- --------------------------------------------------------------------------
   function Successful_Tag return String is
   begin
      case Settings.Output_Format is
         when Settings.NaturalDocs => return "<Successful>";
         when Settings.Markdown    => return "[Successful](tests_status.md#successful)";
      end case;
   end Successful_Tag;

   -- --------------------------------------------------------------------------
   function Line_End return String is
   begin
      case Settings.Output_Format is
         when Settings.NaturalDocs => return "";
         when Settings.Markdown    => return "  ";
            -- "  " to ensure a newline in MD format
      end case;
   end Line_End;

   -- --------------------------------------------------------------------------
   type Indent_Level is range 0 .. 2;
   Current_Indent : Indent_Level := 0;

   -- --------------------------------------------------------------------------
   function Indent return String is
      Indent_1 : constant String := " ";
      Indent_2 : constant String := "  ";
   begin
      case Current_Indent is
         when 0 => return "";
         when 1 => return Indent_1;
         when 2 => return Indent_2;
      end case;
   end Indent;


   -- --------------------------------------------------------------------------
   procedure Put_Usage is
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("   testrec [Options] [Command]");
      New_Line;
      Put_Line ("Command:");
      Put_Line ("   testrec create test_suite_name                 : start the test suite recording");
      Put_Line ("   testrec cmt    ""This is my Comment""            : insert the comments in the log file");
      Put_Line ("   testrec cmt                                    : insert an empty line in the log file");
      Put_Line ("   testrec start  Test_Name                       : start a test");
      Put_Line ("   testrec assert true|false Cmd ""Arg1 Arg2 Arg3"" : check that the command return true or false");
      Put_Line ("   testrec end                                    : end test in progress");
      Put_Line ("   testrec run test_name true|false Cmd ""Arg1 Arg2 Arg3"" : run the test by launching the cmd with Args");
      Put_Line ("   testrec clean                                  : remove testrec tmp files, intended to be used in Makefile's clean section");
      Put_Line ("   testrec state                                  : display testrec current state");
      New_Line;
      Put_Line ("Options:");
      Put_Line ("   -q  | --quiet       : no output on standard output unless error");
      Put_Line ("   -v  | --verbose     : standard output is as verbose as possible");
      Put_Line ("   Note that those options have no effect on text file output (that is always verbose)");
      Put_Line ("   Default standard output behavior:");
      Put_Line ("   - only Test Suite start and Test end are displayed");
      Put_Line ("   - assert, cmt, clean, end, etc. do not output messages");
      Put_Line ("   - state command ignore those options");
      Put_Line ("   -nd | --naturaldocs : use a NaturalDocs friendly txt format instead of the default markdown");
      Put_Line ("   -h  | --help        : this message");
      New_Line;
      Put_Line ("Tests:");
      Put_Line ("   A test is a sequence of call to assert :");
      Put_Line ("   > testrec assert true cmd");
      Put_Line ("   starting with :");
      Put_Line ("   > testrec start");
      Put_Line ("   and endding with :");
      Put_Line ("   > testrec end");
      New_Line;
      Put_Line ("   The test is :");
      Put_Line ("   - Failed if one of the assertion return False;");
      Put_Line ("   - Empty  if no assertion was checked;");
      Put_Line ("   - Successful otherwise.");
      New_Line;
      Put_Line ("Test suite:");
      Put_Line ("   A test suite is a sequence of test, created with a call to");
      Put_Line ("   > testrec create");
      Put_Line ("   All runned test will belong this suite until Creating a test suite is optionnal.");
      Put_Line ("   Creating a test suite is optionnal, you can just directly run tests.");
      New_Line;
      Put_Line ("Short form:");
      Put_Line ("   For test consisting in a single command call, the run command may be convenient.");
      Put_Line ("   A call to :");
      Put_Line ("   > testrec run test_name true|false Cmd ""Arg1 Arg2 Arg3""");
      Put_Line ("   Is equivalent to :");
      Put_Line ("   > testrec start Test_Name");
      Put_Line ("   > testrec assert true|false Cmd ""Arg1 Arg2 Arg3""");
      Put_Line ("   > testrec end");
      New_Line;
      Put_Line ("Persistence:");
      Put_Line ("   Testrec stores its current state, including tests execution informations, in a local .testrec file.");
      Put_Line ("   This file can be display with :");
      Put_Line ("   > testrec state");
      Put_Line ("   .testrec file is deleted when calling :");
      Put_Line ("   > testrec clean");
      Put_Line ("   NB : a single test should be run at a time in a directory.");
      New_Line;
      Put_Line ("Makefile example:");
      Put_Line ("   ./testrec start ""diff test""");
      Put_Line ("   echo ""First file content"" > file1");
      Put_Line ("   echo ""Second file content"" > file2");
      Put_Line ("   ./testrec assert false diff ""file1 file2""");
      Put_Line ("   ./testrec end");
      New_Line;
   end Put_Usage;

   type Verbosity is (Verbose, Default, Quiet);

   -- --------------------------------------------------------------------------
   procedure Put_Line (Text         : String;
                       At_Verbosity : Verbosity := Default) is
   begin
      if (At_Verbosity = Verbose and (Settings.Verbose)) or
        (At_Verbosity = Default and (not Settings.Quiet)) or
        At_Verbosity = Quiet
      then
         Ada.Text_IO.Put_Line (Text);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure New_Line (At_Verbosity : Verbosity := Default) is
   begin
      if (At_Verbosity = Verbose and (Settings.Verbose)) or
        (At_Verbosity = Default and (not Settings.Quiet)) or
        At_Verbosity = Quiet
      then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   procedure Create_Suite (Name : String) is
   begin
      Current_Indent := 0;

      -- Std output:
      New_Line;

      Put_Line (Indent & "Starting test suite : " & Name, Verbose);
      New_Line (Verbose);

      -- Log file:
      declare
         use Ada.Text_IO;
      begin
         New_Line (Log_File);
         case Settings.Output_Format is
            when Settings.NaturalDocs =>
               Put_Line (Log_File, Indent & "Test Suite: " & Name);
            when Settings.Markdown    =>
               Put_Line (Log_File, "# " & Name);
               New_Line (Log_File);
         end case;
         New_Line (Log_File);
      end;

      -- State File:
      Current_State.Create_Suite (Name);

   end Create_Suite;

   -- -------------------------------------------------------------------------
   procedure Cmt (Text : String := "") is
   begin
      declare
         use Ada.Text_IO;
         use Settings;
      begin
         -- Log file:
         if Text = "" then
            New_Line (Log_File);
         elsif Text = "```" and Settings.Output_Format = Settings.Markdown then
            -- In MD format, code marker ``` should not be indented
            Arg_Counter := Arg_Counter + 1;
            Put_Line (Log_File, Text & Line_End);
         else
            Arg_Counter := Arg_Counter + 1;
            Put_Line (Log_File, Indent & Text & Line_End);
         end if;
      end;

      -- Std output:
      if Text = "" then
         New_Line (Verbose);
      else
         Arg_Counter := Arg_Counter + 1;
         Put_Line (Indent & Text, Verbose);
      end if;
   end Cmt;

   -- -------------------------------------------------------------------------
   procedure Clean is
   begin
      -- Std output:
      Put_Line ("Cleaning Testrec state...", Verbose);
      New_Line (Verbose);

      -- State File:
      Current_State.Clean;
   end Clean;

   -- -------------------------------------------------------------------------
   procedure Assert (Expected :     Boolean;
                     Prog     :     String;
                     Opts     :     String;
                     Success  : out Boolean)
   is
      -- Return_Code   : Integer := 0;
      use GNAT.OS_Lib;
      Spawn_Success : Boolean;

   begin
      -- Version 1 : no return code needed:
      Spawn (Program_Name => Prog,
             Args         => Argument_String_To_List (Opts).all,
             Success      => Spawn_Success);
      Success := Expected = Spawn_Success;

      -- -- Version 2 : return code needed:
      --        Spawn (Program_Name           => Prog,
      --               Args                   => Argument_String_To_List (Opts).all,
      --               Output_File_Descriptor => Standout,
      --               Return_Code            => Return_Code,
      --               Err_To_Out             => True);
      --        Success := Expected = (Return_Code = 0);
      --  Ada.Text_IO.Put_Line ("Success = " & Boolean'Image (Success) & ", Return code = " & Integer'Image (Return_Code));

      -- State File:
      Current_State.Assert (Success);

      -- Std output:
      if Success then
         Put_Line (Indent & "- Asserting that """ & Prog & " " & Opts & """ return " & Boolean'Image (Expected) & " : " & "OK", Verbose);

      else
         Put_Line (Indent & "- Asserting that """ & Prog & " " & Opts & """ return " & Boolean'Image (Expected) & " : " & "Failed", Verbose);

      end if;

      --        -- Log file:
      --        declare
      --           use Ada.Text_IO;
      --           Common_Text : constant String := Indent & "- Asserting that """ & Prog & " " & Opts & """ return " & Boolean'Image (Expected);
      --        begin
      --           if Success then
      --              Put_Line (Log_File, Item => Common_Text & " : OK");
      --           else
      --              Put_Line (Log_File, Item => "*" & Common_Text & " : Failed*");
      --           end if;
      --        end;

   end Assert;

   -- --------------------------------------------------------------------------
   procedure State is
   begin
      Put_Line ("Testrec current state :", Quiet);
      New_Line (Quiet);
      Put_Line (Current_State.State_String, Quiet);
   end State;

   -- --------------------------------------------------------------------------
   procedure Start (Test_Name : in String) is
      use Ada.Strings.Unbounded;
      Common_Text : Unbounded_String;
   begin
      Current_Indent := 1;

      if Current_State.Suite.Suite_Name = Null_Unbounded_String then
         Common_Text := Indent &                                          To_Unbounded_String (Test_Name);
      else
         Common_Text := Indent & Current_State.Suite.Suite_Name & " / " & To_Unbounded_String (Test_Name);
      end if;

      -- Std output:
      Put_Line (Indent & "Starting test " & Test_Name, Verbose);

      -- State File:
      Current_State.Start_Test (Test_Name);

      -- Log file:
      declare
         use Ada.Text_IO;
      begin
         New_Line (Log_File);
         case Settings.Output_Format is
            when Settings.NaturalDocs =>
               Put_Line (Log_File, Item => Indent & "Test: " & To_String (Common_Text));
            when Settings.Markdown    =>
               Put_Line (Log_File, Item =>          "## " & To_String (Common_Text));
               New_Line (Log_File);
         end case;
      end;

      Current_Indent := 2;
      -- comment or Assert following should be indented respect to Test title

   end Start;

   -- --------------------------------------------------------------------------
   procedure End_Test is
      use Current_State;
      use Ada.Strings.Unbounded;
      Common_Text : Unbounded_String;

   begin
      Current_Indent := 0;
      -- Test conclusion is at the same level than Test title

      if Current_State.Suite.Suite_Name = Null_Unbounded_String then
         Common_Text := Indent &                                          Current_State.Test.Test_Name & " ";
      else
         Common_Text := Indent & Current_State.Suite.Suite_Name & " / " & Current_State.Test.Test_Name & " ";
      end if;


      if Current_State.Test.Status = Test_Declared then
         -- test declared, but no assertion done:
         Put_Line (To_String (Common_Text) & "[Empty]", Quiet);
         declare
            Text : constant String := To_String (Common_Text) & Empty_Tag;
         begin
            Ada.Text_IO.New_Line (Log_File);
            Ada.Text_IO.Put_Line (Log_File, Item => Text);
         end;

      elsif Current_State.Test.Status = Test_In_Progress then
         if Current_State.Test.Result = Successful then
            Put_Line (To_String (Common_Text) & "[Successful]", Quiet);
            declare
               Text : constant String := To_String (Common_Text) & Successful_Tag;
            begin
               Ada.Text_IO.New_Line (Log_File);
               Ada.Text_IO.Put_Line (Log_File, Item => Text);
            end;

         elsif Current_State.Test.Result = Failed then
            Put_Line (To_String (Common_Text) & "[Failed]", Quiet);
            declare
               Text : constant String := To_String (Common_Text) & Fail_Tag;
            begin
               Ada.Text_IO.New_Line (Log_File);
               case Settings.Output_Format is
               when Settings.NaturalDocs =>
                  Ada.Text_IO.Put_Line (Log_File, Item => "*"  & Text & "*");
               when Settings.Markdown    =>
                  Ada.Text_IO.Put_Line (Log_File, Item => "**" & Text & "**");
               end case;
            end;
         end if;
      end if;

      -- State File:
      Current_State.End_Test;

   end End_Test;

   -- --------------------------------------------------------------------------
   procedure Run (Test_Name : String;
                  Expected  : Boolean;
                  Prog      : String;
                  Opts      : String;
                  Success   : out Boolean) is
   begin
      Start (Test_Name);
      Assert (Expected, Prog, Opts, Success);
      End_Test;
   end Run;

   -- --------------------------------------------------------------------------
   procedure Open_Log_File is
      use Ada.Text_IO;
   begin
      if Ada.Directories.Exists (Settings.Log_File_Name) then
         Open (Name => Settings.Log_File_Name, File => Log_File, Mode => Append_File);
      else
         -- Let's create an initial log file:
         Create (Log_File, Out_File, Settings.Log_File_Name);
      end if;
   end Open_Log_File;

   -- -------------------------------------------------------------------------
   procedure Set_Indent_Level is
      use Current_State;
      use Ada.Strings.Unbounded;
   begin
      -- Positionning the indent level, and that's not so simple
      case Test.Status is
         when No_Test_In_Progress => Current_Indent := 0;
         when Test_Declared       => Current_Indent := 1;
         when Test_In_Progress    => Current_Indent := 1;
         when Test_Done           => Current_Indent := 0;
      end case;
      if Suite.Suite_Name /= Null_Unbounded_String then
         Current_Indent := Current_Indent + 1;
      end if;
   end Set_Indent_Level;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Usage;

   else
      Open_Log_File;

      Set_Indent_Level;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "create" then
               Create_Suite (Name => Ada.Command_Line.Argument (Arg_Counter + 1));
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "cmt" then
               if Arg_Counter = Ada.Command_Line.Argument_Count then
                  Cmt;
               else
                  Cmt (Ada.Command_Line.Argument (Arg_Counter + 1));
                  Arg_Counter := Arg_Counter + 1;
               end if;

            elsif Opt = "clean" then
               Clean;

            elsif Opt = "assert" then
               declare
                  Success : Boolean;
               begin
                  Assert (Expected => Boolean'Value (Ada.Command_Line.Argument (Arg_Counter + 1)),
                          Prog     => Ada.Command_Line.Argument (Arg_Counter + 2),
                          Opts     => Ada.Command_Line.Argument (Arg_Counter + 3),
                          Success  => Success);
                  Arg_Counter := Arg_Counter + 3;
                  if Success then
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
                  else
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  end if;
               end;

            elsif Opt = "state" then
               State;

            elsif Opt = "start" then
               Start (Test_Name => Ada.Command_Line.Argument (Arg_Counter + 1));
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "end" then
               End_Test;

            elsif Opt = "run" then
               declare
                  Success : Boolean;
               begin
                  Run (Test_Name   => Ada.Command_Line.Argument (Arg_Counter + 1),
                       Expected    => Boolean'Value (Ada.Command_Line.Argument (Arg_Counter + 2)),
                       Prog        => Ada.Command_Line.Argument (Arg_Counter + 3),
                       Opts        => Ada.Command_Line.Argument (Arg_Counter + 4),
                       Success     => Success);
                  if Success then
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
                  else
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  end if;
                  Arg_Counter := Arg_Counter + 4;
               end;

            elsif Opt = "-h" or Opt = "--help" then
               Put_Usage;

            elsif Opt = "-v" or Opt = "--verbose" then
               Settings.Verbose := True;

            elsif Opt = "-q" or Opt = "--quiet" then
               Settings.Quiet := True;

            elsif Opt = "-nd" or Opt = "--naturaldocs" then
               Settings.Output_Format := Settings.NaturalDocs;

            else
               Put_Line ("Unknown Option " & Opt, Quiet);
               Put_Usage;
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

            end if;

         end;
         Arg_Counter := Arg_Counter + 1;
      end loop;

      Ada.Text_IO.Close (Log_File);

   end if;

exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Ada.Exceptions.Exception_Information (Error));

end Testrec.Main;
