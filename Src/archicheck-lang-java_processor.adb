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
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Units;              use Archicheck.Units;
with Archicheck.Settings;

with Java_Lexer;
with OpenToken.Text_Feeder.Text_IO;

with Ada.Exceptions;
with Ada.Text_IO;

package body Archicheck.Lang.Java_Processor is

   -- --------------------------------------------------------------------------
   Processor : aliased Java_Interface;

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Language_Processor => Processor'Access,
                For_Language       => Sources.Java);
   end Initialize;

   -- --------------------------------------------------------------------------
   function File_Extensions (Lang : in Java_Interface) return String is
      pragma Unreferenced (Lang);
   begin
      return Settings.Java_Files_Pattern;
   end File_Extensions;

   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in Java_Interface;
                                   From_Source : in Sources.File_Name)
   is
      pragma Unreferenced (Lang);

      -- Change default Debug parameter value to enable/disable
      -- Debug messages in this package
      -- -----------------------------------------------------------------------
      -- procedure Put_Debug_Line
      --   (Msg    : in String  := "";
      --    Debug  : in Boolean := Settings.Debug_Mode;
      --    Prefix : in String  := "Java_Processor.Analyze_Dependencies")
      --    renames Archicheck.IO.Put_Debug_Line;
      -- pragma Unreferenced (Put_Debug_Line);

      -- Global text file for reading parse data
      File : Ada.Text_IO.File_Type;

      Dep_List : Units.Dependency_Targets.List :=
                   Units.Dependency_Targets.Empty_List;

      use Java_Lexer;

      -- -----------------------------------------------------------------------
      function Current_Location return Sources.Location is
        (File   => From_Source,
         Line   => Java_Lexer.Analyzer.Line,
         Column => 0) with Inline;

      -- -----------------------------------------------------------------------
      -- Procedure: Get_Unit_Name
      -- -----------------------------------------------------------------------
      function Get_Unit_Name return Unit_Name is
         Name : Unit_Name := +Analyzer.Lexeme;
      begin
         loop
            Analyzer.Find_Next;
            if Analyzer.ID = Dot_T then
               Analyzer.Find_Next;
               Name := Name & "." & Analyzer.Lexeme;
            else
               exit;
            end if;
         end loop;
         return Name;
      end Get_Unit_Name;

      Unit_Kind : Units.Java_Unit_Kind;
      Pkg_Name  : Unit_Name := Null_Unit_Name;

      use Sources;

   begin
      if Settings.Debug_Mode then OpenToken.Trace_Parse := 1; end if;
      -- value > 0 => debug level

      IO.Put_Line ("Looking for dependencies in " & (+From_Source) & " :",
                   Level => IO.Verbose);

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => +From_Source);
      Ada.Text_IO.Set_Input (File);
      Analyzer.Reset;
      Analyzer.Set_Text_Feeder
        (OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Current_Input));

      Source_Analysis : loop
         case Analyzer.ID is
            when Package_T =>
               -- processing the package declaration
               --     > package Java.Awt.Evt;
               -- line
               Analyzer.Find_Next;
               Pkg_Name := Get_Unit_Name;

            when Import_T =>
               -- processing a
               --     > import [static] Java.Awt.Evt;
               --     > import [static] Java.Awt.*;
               -- line
               --
               -- Cf. (for example) to
               -- https://stackoverflow.com/questions/20777260/java-import-statement-syntax
               -- for a discussion on package/class name in Java

               Analyzer.Find_Next;
               if Analyzer.ID = Static_T then
                  Analyzer.Find_Next;
               end if;

               declare
                  Withed_Unit : constant Unit_Name := Get_Unit_Name;
               begin
                  IO.Put_Line ("   - depends on " & (+Withed_Unit),
                               Level => IO.Verbose);

                  Dep_List.Append ((To_Unit  => Withed_Unit,
                                    Location => Current_Location));
                  -- Only one unit name per import statement in Java,
                  -- no need to loop.
               end;

            when Class_T | Interface_T =>
               -- processing the class declaration
               --     > [public] [abstract] [class|interface] ... ;
               -- line
               if    Analyzer.ID = Class_T     then Unit_Kind := Units.Class_K;
               elsif Analyzer.ID = Interface_T then Unit_Kind := Units.Interface_K;
               end if;

               Analyzer.Find_Next;
               declare
                  From      : constant Unit_Name := Get_Unit_Name;
                  Full_Name : Unit_Name;

               begin
                  if Pkg_Name = Null_Unit_Name then
                     -- no Package, let's fall back on the class name
                     Full_Name := From;
                  else
                     Full_Name := Pkg_Name & '.' & From;
                  end if;

                  IO.Put_Line
                    ("   - defines " & Units.Unit_Kind'Image (Unit_Kind) &
                       " " & (+Full_Name),
                     Level => IO.Verbose);
                  case Unit_Kind is
                     when Units.Class_K =>
                        Archicheck.Units.Add_Unit
                          (Unit    => (Name           => Full_Name,
                                       Lang           => Sources.Java,
                                       Kind           => Units.Class_K,
                                       Implementation => True),
                           Targets => Dep_List);
                     when Units.Interface_K =>
                        Archicheck.Units.Add_Unit
                          (Unit    => (Name           => Full_Name,
                                       Lang           => Sources.Java,
                                       Kind           => Units.Interface_K,
                                       Implementation => True),
                           Targets => Dep_List);
                  end case;

               end;

               exit Source_Analysis;
               -- Optimization : this exit cause the analysis to be stop
               -- after discovery of the first class or interface.
               --
               -- According to Java standards and common Java practices,
               -- every class stands in its own source file.
               -- However, it is possible to have multiple class in a single
               -- file, provided that there is only one public.
               -- All the top-level non-public types will be package private.
               --
               -- Processing private classes is by definition of no interest
               -- for ArchiCheck, so there is no problem in exiting here.
               --
               -- On the other hand, it is possible to import the public
               -- nested classes of an enclosing class.
               -- Not sure what can be the consequences of processing such an
               -- import, but not processing nested classes in this parser.
               -- But anyway, as it would be costly in time processing, and
               -- would raise far more complex the lexer, I don't intend to
               -- change this code for now.
               --

               -- when End_of_File_T => exit Source_Analysis;

               when others => Analyzer.Find_Next;

         end case;

         exit Source_Analysis when Analyzer.ID = End_of_File_T;

      end loop Source_Analysis;

      Ada.Text_IO.Close (File => File);

   exception
      when Error : others =>
         IO.Put_Exception
           (Sources.Location_Image (Current_Location) & "parse exception:");
         IO.Put_Exception (Ada.Exceptions.Exception_Information (Error));
         Ada.Text_IO.Close (File => File);

   end Analyze_Dependencies;


end Archicheck.Lang.Java_Processor;
