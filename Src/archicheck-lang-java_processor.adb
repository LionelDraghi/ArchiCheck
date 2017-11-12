-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Java_Processor body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--   - More more thorough Ada implementation needed
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Settings;
with Archicheck.Dependencies;

with Java_Lexer;
with OpenToken.Text_Feeder.Text_IO;

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Archicheck.Lang.Java_Processor is

   -- --------------------------------------------------------------------------
   Processor : aliased Java_Interface;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Subscribe (Language_Processor => Processor'Access,
                 For_Language       => Sources.Java);
   end Initialize;

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   function File_Extensions (Lang : in Java_Interface) return String is
      pragma Unreferenced (Lang);
   begin
      return Settings.Java_Files_Pattern;
   end File_Extensions;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   --
   -- Implementation Notes:
   --   - Based on OpenToken Java_Lexer
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in Java_Interface;
                                   From_Source : in String) is
      pragma Unreferenced (Lang);

      -- Change default Debug parameter value to enable/disable Debug messages in this package
      -- -----------------------------------------------------------------------
      procedure Put_Debug_Line (Msg    : in String  := "";
                                Debug  : in Boolean := Settings.Debug_Mode;
                                Prefix : in String  := "Java_Processor.Analyze_Dependencies") renames Archicheck.IO.Put_Debug_Line;
--        procedure Put_Debug (Msg    : in String  := "";
--                             Debug  : in Boolean := Settings.Debug_Mode;
--                             Prefix : in String  := "Java_Processor.Analyze_Dependencies") renames Archicheck.IO.Put_Debug;
      -- procedure New_Debug_Line (Debug  : in Boolean := Settings.Debug_Mode) renames Archicheck.IO.New_Debug_Line;

      -- Global text file for reading parse data
      File : Ada.Text_IO.File_Type;

      use Dependencies;
      use Dependencies.Dependency_Lists;
      Tmp  : Dependency_Lists.List := Dependency_Lists.Empty_List;

      use Ada.Strings.Unbounded;
      use Java_Lexer;

      -- -----------------------------------------------------------------------
      -- Procedure: Get_Unit_Name
      -- -----------------------------------------------------------------------
      function Get_Unit_Name return String is
         Name : Unbounded_String := Null_Unbounded_String;
      begin
         Name := To_Unbounded_String (Analyzer.Lexeme);
         loop
            Analyzer.Find_Next;
            if Analyzer.ID = Dot_T then
               Analyzer.Find_Next;
               Name := Name & "." & Analyzer.Lexeme;
            else
               exit;
            end if;
         end loop;
         return To_String (Name);
      end Get_Unit_Name;

      -- -----------------------------------------------------------------------
      -- Procedure: Set_Unit_Name
      -- -----------------------------------------------------------------------
      -- Iterate trough a list to set the field Unit_Name, and only this one.
--        procedure Set_Unit_Name (List      : in out Dependency_Lists.List;
--                                 Unit_Name : in     String;
--                                 Kind      : in     Unit_Kind) is
--        begin
--           for Dep of List loop
--              Dep.From.Name := To_Unbounded_String (Unit_Name);
--              Dep.From.Kind := Kind;
--           end loop;
--        end Set_Unit_Name;

      -- Found_Pkg  : Unbounded_String := To_Unbounded_String ("Default");
      -- if the is not package declaration before import declarations, the class is owned by Default package

      Unit_Kind : Dependencies.Unit_Kind;

   begin
      -- New_Debug_Line;
      IO.Put_Line ("Looking for dependencies in " & From_Source & " :", Only_When_Verbose => True);

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => From_Source);
      Ada.Text_IO.Set_Input (File);
      Analyzer.Set_Text_Feeder (OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Current_Input));


      Source_Analysis : loop
         begin
            case Analyzer.ID is
            when Package_T =>
               -- processing the package declaration
               --     > package Java.Awt.Evt;
               -- line
               Put_Debug_Line ("Analyzing Java pkg " & Java_Token'Image (Analyzer.ID));
               Analyzer.Find_Next;
               --                 declare
               --                    Unit : constant String := Get_Unit_Name;
               --                 begin
               --                    Unit_Type_Identified := True;
               --                 end;

            when Import_T =>
               -- processing a
               --     > import [static] Java.Awt.Evt;
               -- line
               Unit_List : loop
                  Analyzer.Find_Next;
                  if Analyzer.ID = Static_T then
                     Analyzer.Find_Next;
                  end if;

                  declare
                     Withed_Unit : constant String := Get_Unit_Name;
                  begin
                     IO.Put_Line ("   - " & Withed_Unit, Only_When_Verbose => True);
                     Append (Tmp, (From => (Name => Null_Unbounded_String,
                                            File => To_Unbounded_String (From_Source),
                                            Lang => Sources.Java,
                                            Kind => Java_Class),
                                   To   => (Name => To_Unbounded_String (Withed_Unit),
                                            File => Null_Unbounded_String,
                                            Lang => Sources.Java,
                                            Kind => Java_Class)));
                     exit Unit_List when Analyzer.ID /= Comma_T;
                     --** not sure it's possible to have several unit behind a single import in Java
                     -- otherwise, loop to continue in the list of comma separated withed unit
                  end;
               end loop Unit_List;

            when Class_T | Interface_T =>
               -- processing the class declaration
               --     > [public] [class|interface] ;
               -- line
               if    Analyzer.ID = Class_T     then Unit_Kind := Dependencies.Java_Class;
               elsif Analyzer.ID = Interface_T then Unit_Kind := Dependencies.Java_Interface;
               end if;

               Analyzer.Find_Next;

               declare
                  From : constant String := Get_Unit_Name;
               begin
                  for D of Tmp loop
                     Dependencies.Append ((From => (Name => To_Unbounded_String (From),
                                                    File => D.From.File,
                                                    Lang => D.From.Lang,
                                                    Kind => Unit_Kind),
                                           To   => D.To));
                  end loop;
               end;

               exit Source_Analysis;
               -- this exit is a risky optimization, as I am not sure it is possible to have multiple class in a single file...

               when others => Analyzer.Find_Next;

            end case;

            exit Source_Analysis when Analyzer.ID = End_of_File_T;

         exception
            when Error : others =>
               IO.Put_Error
                 (IO.GNU_Prefix (From_Source, Analyzer.Line, Analyzer.Column) & "parse exception:");
               IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
         end;

      end loop Source_Analysis;

      Ada.Text_IO.Close (File => File);

   end Analyze_Dependencies;

end Archicheck.Lang.Java_Processor;
