-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Config;
with Ada.Text_IO; use Ada.Text_IO;

with Testrec.Settings;

-- -------------------------------------------------------------------------
package body Testrec.Current_State is

   use Config;

   State           : Config.Configuration;
   State_File      : Ada.Text_IO.File_Type;

   Overall_State_Mark : constant String := "Overall_State";
   Suite_State_Mark   : constant String := "Current_Suite_State";
   Test_State_Mark    : constant String := "Current_Test_State";

   Suite_Count_Mark                : constant String := "Suite_Count";
   Failed_Test_Count_Mark          : constant String := "Failed_Test_Count";
   Successful_Test_Count_Mark      : constant String := "Successful_Test_Count";
   Empty_Test_Count_Mark           : constant String := "Empty_Test_Count";
   Failed_Assertion_Count_Mark     : constant String := "Failed_Assertion_Count";
   Successful_Assertion_Count_Mark : constant String := "Successful_Assertion_Count";
   Suite_Name_Mark                 : constant String := "Current_Suite_Name";
   Test_Name_Mark                  : constant String := "Current_Test_Name";
   Test_Status_Mark                : constant String := "Current_Test_Status";
   Test_Result_Mark                : constant String := "Current_Test_Result";

   -- ----------------------------------------------------------------------
   function State_String (Overall : Overall_State) return String is
   begin
      return
        "   " & Suite_Count_Mark                & "=" & Natural'Image (Overall.Suite_Count)                & LF &
        "   " & Failed_Test_Count_Mark          & "=" & Natural'Image (Overall.Failed_Test_Count)          & LF &
        "   " & Successful_Test_Count_Mark      & "=" & Natural'Image (Overall.Successful_Test_Count)      & LF &
        "   " & Empty_Test_Count_Mark           & "=" & Natural'Image (Overall.Empty_Test_Count)           & LF &
        "   " & Failed_Assertion_Count_Mark     & "=" & Natural'Image (Overall.Failed_Assertion_Count)     & LF &
        "   " & Successful_Assertion_Count_Mark & "=" & Natural'Image (Overall.Successful_Assertion_Count) & LF & LF;
   end State_String;

   -- ----------------------------------------------------------------------
   function State_String (Suite : Suite_State) return String is
   begin
      return
        "   " & Suite_Name_Mark                 & "= " & To_String     (Suite.Suite_Name)                 & LF &
        "   " & Failed_Test_Count_Mark          & "="  & Natural'Image (Suite.Failed_Test_Count)          & LF &
        "   " & Successful_Test_Count_Mark      & "="  & Natural'Image (Suite.Successful_Test_Count)      & LF &
        "   " & Empty_Test_Count_Mark           & "="  & Natural'Image (Suite.Empty_Test_Count)           & LF &
        "   " & Failed_Assertion_Count_Mark     & "="  & Natural'Image (Suite.Failed_Assertion_Count)     & LF &
        "   " & Successful_Assertion_Count_Mark & "="  & Natural'Image (Suite.Successful_Assertion_Count) & LF & LF;
   end State_String;

   -- ----------------------------------------------------------------------
   function State_String (Test : Test_State) return String is
   begin
      return
        "   " & Test_Name_Mark                  & "= " & To_String         (Test.Test_Name)                  & LF &
        "   " & Failed_Assertion_Count_Mark     & "="  & Natural'Image     (Test.Failed_Assertion_Count)     & LF &
        "   " & Successful_Assertion_Count_Mark & "="  & Natural'Image     (Test.Successful_Assertion_Count) & LF &
        "   " & Test_Status_Mark                & "= " & Test_Status'Image (Test.Status)                     & LF &
        "   " & Test_Result_Mark                & "= " & Test_Result'Image (Test.Result)                     & LF & LF;
   end State_String;

   Default_Overall_State : constant Overall_State :=
                             (Suite_Count                => 0,
                              Failed_Test_Count          => 0,
                              Successful_Test_Count      => 0,
                              Empty_Test_Count           => 0,
                              Failed_Assertion_Count     => 0,
                              Successful_Assertion_Count => 0);
   Default_Suite_State   : constant Suite_State :=
                             (Suite_Name                 => Null_Unbounded_String,
                              Failed_Test_Count          => 0,
                              Successful_Test_Count      => 0,
                              Empty_Test_Count           => 0,
                              Failed_Assertion_Count     => 0,
                              Successful_Assertion_Count => 0);
   Default_Test_State    : constant Test_State :=
                          (Test_Name                  => Null_Unbounded_String,
                           Failed_Assertion_Count     => 0,
                           Successful_Assertion_Count => 0,
                           Result                     => Empty,
                           Status                     => No_Test_In_Progress);

   Current_Overall_State : Overall_State := Default_Overall_State;
   Current_Suite_State   : Suite_State   := Default_Suite_State;
   Current_Test_State    : Test_State    := Default_Test_State;

   -- ----------------------------------------------------------------------
   function State_String return String is
   begin
      return
        "[" & Overall_State_Mark & "]" & LF & State_String (Current_Overall_State) &
        "[" & Suite_State_Mark   & "]" & LF & State_String (Current_Suite_State) &
        "[" & Test_State_Mark    & "]" & LF & State_String (Current_Test_State);
   end State_String;

   -- ----------------------------------------------------------------------
   function Overall return Overall_State is
   begin
      return Current_Overall_State;
   end Overall;
   function Suite   return Suite_State is
   begin
      return Current_Suite_State;
   end Suite;
   function Test    return Test_State is
   begin
      return Current_Test_State;
   end Test;

   -- ----------------------------------------------------------------------
   procedure Update_State is
   begin
      State.Replace_Section (Section      => Overall_State_Mark,
                             New_Contents => State_String (Current_Overall_State));
      State.Replace_Section (Section      => Suite_State_Mark,
                             New_Contents => State_String (Current_Suite_State));
      State.Replace_Section (Section      => Test_State_Mark,
                             New_Contents => State_String (Current_Test_State));
   end Update_State;

   -- ----------------------------------------------------------------------
   procedure Clean is
   begin
      Ada.Directories.Delete_File (Settings.State_File_Name);
      Ada.Directories.Delete_File (Settings.Log_File_Name);
   end Clean;

   -- ----------------------------------------------------------------------
   procedure Create_Suite (Name : String) is
   begin
      Current_Overall_State.Suite_Count := Current_Overall_State.Suite_Count + 1;
      Current_Suite_State :=
        (Suite_Name                 => To_Unbounded_String (Name),
         Failed_Test_Count          => 0,
         Successful_Test_Count      => 0,
         Empty_Test_Count           => 0,
         Failed_Assertion_Count     => 0,
         Successful_Assertion_Count => 0);
      Current_Test_State := Default_Test_State;
      Update_State;
   end Create_Suite;

   -- ----------------------------------------------------------------------
   procedure Start_Test (Name : String) is
   begin
      End_Test;
      Current_Test_State :=
        (Test_Name                  => To_Unbounded_String (Name),
         Failed_Assertion_Count     => 0,
         Successful_Assertion_Count => 0,
         Result                     => Empty,
         Status                     => Test_Declared);
      Update_State;
   end Start_Test;

   -- ----------------------------------------------------------------------
   procedure Assert (Assertion : Boolean) is
   begin
      Current_Test_State.Status := Test_In_Progress;
      if Assertion then
         Current_Overall_State.Successful_Assertion_Count := Current_Overall_State.Successful_Assertion_Count + 1;
         Current_Suite_State.Successful_Assertion_Count   := Current_Suite_State.Successful_Assertion_Count + 1;
         Current_Test_State.Successful_Assertion_Count    := Current_Test_State.Successful_Assertion_Count + 1;
         Current_Test_State.Result                        := Successful;
      else
         Current_Overall_State.Failed_Assertion_Count := Current_Overall_State.Failed_Assertion_Count + 1;
         Current_Suite_State.Failed_Assertion_Count   := Current_Suite_State.Failed_Assertion_Count + 1;
         Current_Test_State.Failed_Assertion_Count    := Current_Test_State.Failed_Assertion_Count + 1;
         Current_Test_State.Result                    := Failed;
      end if;
      Update_State;
   end Assert;

   -- ----------------------------------------------------------------------
   procedure End_Test is
   begin
      if Current_Test_State.Status = Test_Declared then
         -- test declared, but no assertion done:
         Current_Overall_State.Empty_Test_Count := Current_Overall_State.Empty_Test_Count + 1;
         Current_Suite_State.Empty_Test_Count   := Current_Suite_State.Empty_Test_Count + 1;

      elsif Current_Test_State.Status = Test_In_Progress then
         -- a test was in progress, let's update counts:
         if Current_Test_State.Result = Successful then
            Current_Overall_State.Successful_Test_Count := Current_Overall_State.Successful_Test_Count + 1;
            Current_Suite_State.Successful_Test_Count   := Current_Suite_State.Successful_Test_Count + 1;
         elsif Current_Test_State.Result = Failed then
            Current_Overall_State.Failed_Test_Count := Current_Overall_State.Failed_Test_Count + 1;
            Current_Suite_State.Failed_Test_Count   := Current_Suite_State.Failed_Test_Count + 1;
         end if;
      end if;
      Current_Test_State.Status := Test_Done;
      Update_State;
   end End_Test;

begin
   if not Ada.Directories.Exists (Settings.State_File_Name) then
      -- Let's create an initial state file:
      Create (State_File, Out_File, Settings.State_File_Name);
      Put_Line (State_File, State_String);
      Close (State_File);
   end if;

   -- Let's load values from the existing state file:
   State := Init (Settings.State_File_Name, Variable_Terminator => '=', On_Type_Mismatch => Print_Warning);

   Current_Overall_State :=
     (Suite_Count                => State.Value_Of (Overall_State_Mark, Suite_Count_Mark,                Default => 0),
      Failed_Test_Count          => State.Value_Of (Overall_State_Mark, Failed_Test_Count_Mark,          Default => 0),
      Successful_Test_Count      => State.Value_Of (Overall_State_Mark, Successful_Test_Count_Mark,      Default => 0),
      Empty_Test_Count           => State.Value_Of (Overall_State_Mark, Empty_Test_Count_Mark,           Default => 0),
      Failed_Assertion_Count     => State.Value_Of (Overall_State_Mark, Failed_Assertion_Count_Mark,     Default => 0),
      Successful_Assertion_Count => State.Value_Of (Overall_State_Mark, Successful_Assertion_Count_Mark, Default => 0));
   Current_Suite_State   :=
     (Suite_Name                 => To_Unbounded_String (State.Value_Of (Suite_State_Mark, Suite_Name_Mark,                 Default => "")),
      Failed_Test_Count          =>                      State.Value_Of (Suite_State_Mark, Failed_Test_Count_Mark,          Default => 0),
      Successful_Test_Count      =>                      State.Value_Of (Suite_State_Mark, Successful_Test_Count_Mark,      Default => 0),
      Empty_Test_Count           =>                      State.Value_Of (Suite_State_Mark, Empty_Test_Count_Mark,           Default => 0),
      Failed_Assertion_Count     =>                      State.Value_Of (Suite_State_Mark, Failed_Assertion_Count_Mark,     Default => 0),
      Successful_Assertion_Count =>                      State.Value_Of (Suite_State_Mark, Successful_Assertion_Count_Mark, Default => 0));
   Current_Test_State    :=
     (Test_Name                  => To_Unbounded_String (State.Value_Of (Test_State_Mark, Test_Name_Mark,                  Default => "")),
      Failed_Assertion_Count     =>                      State.Value_Of (Test_State_Mark, Failed_Assertion_Count_Mark,     Default => 0),
      Successful_Assertion_Count =>                      State.Value_Of (Test_State_Mark, Successful_Assertion_Count_Mark, Default => 0),
      Result                     => Test_Result'Value   (State.Value_Of (Test_State_Mark, Test_Result_Mark,                Default => Test_Result'Image (Empty))),
      Status                     => Test_Status'Value   (State.Value_Of (Test_State_Mark, Test_Status_Mark,                Default => Test_Status'Image (No_Test_In_Progress))));

end Testrec.Current_State;
