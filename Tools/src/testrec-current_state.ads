-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Testrec.Current_State is

   pragma Elaborate_Body;

   procedure Clean;

   procedure Create_Suite (Name      : String);
   procedure Start_Test   (Name      : String);
   procedure Assert       (Assertion : Boolean);
   procedure End_Test;

   type Overall_State is record
      Suite_Count                : Natural;
      Failed_Test_Count          : Natural;
      Successful_Test_Count      : Natural;
      Empty_Test_Count           : Natural;
      Failed_Assertion_Count     : Natural;
      Successful_Assertion_Count : Natural;
   end record;

   type Suite_State is record
      Suite_Name                 : Unbounded_String;
      Failed_Test_Count          : Natural;
      Successful_Test_Count      : Natural;
      Empty_Test_Count           : Natural;
      Failed_Assertion_Count     : Natural;
      Successful_Assertion_Count : Natural;
   end record;

   type Test_Result is (Failed, Empty, Successful);
   type Test_Status is (No_Test_In_Progress, Test_Declared, Test_In_Progress, Test_Done);

   type Test_State is record
      Test_Name                  : Unbounded_String;
      Failed_Assertion_Count     : Natural;
      Successful_Assertion_Count : Natural;
      Result                     : Test_Result;
      Status                     : Test_Status;
   end record;

   function State_String return String;

   function Overall return Overall_State;
   function Suite   return Suite_State;
   function Test    return Test_State;

end Testrec.Current_State;
