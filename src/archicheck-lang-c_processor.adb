-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.C_Processor body
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
use Ada.Strings;

package body Archicheck.Lang.C_Processor is

   -- --------------------------------------------------------------------------
   Processor : aliased C_Interface;

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Language_Processor => Processor'Access,
                For_Language       => Sources.C);
   end Initialize;

   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in C_Interface;
                                   From_Source : in Sources.File_Name)
   is
      pragma Unreferenced (Lang);

        -- Change default Debug parameter value to enable/disable
        -- Debug messages in this package
        -- -----------------------------------------------------------------------
        procedure Put_Debug_Line
         (Msg    : in String  := "";
          Debug  : in Boolean := Settings.Debug_Mode;
          Prefix : in String  := "C_Processor.Analyze_Dependencies")
          renames Archicheck.IO.Put_Debug_Line;
        -- pragma Unreferenced (Put_Debug_Line);

      -- Global text file for reading parse data
      File : Ada.Text_IO.File_Type;

      Dep_List : Units.Dependency_Targets.List :=
        Units.Dependency_Targets.Empty_List;

      Col : Natural := 1; -- stores the current column during line analisys

      use Ada.Text_IO;

      -- -----------------------------------------------------------------------
      function Current_Location return Sources.Location is
        (File   => From_Source,
         Line   => Positive (Line (File)),
         Column => Col) with Inline;

      use Sources;

      Source    : constant String := +From_Source;
      Extension : constant String := Tail (Source, Count => 2);
      Unit_Name : constant String := Source (Index (Source, 
                                                    Pattern => "/", 
                                                    Going   => Backward) + 1 .. Source'Length - 2); 
      WTF : exception;

   begin
      IO.Put_Line ("Looking for dependencies in " & Unit_Name & " :",
                   Level => IO.Verbose);

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => +From_Source);
      
      Analysis : while not End_Of_File (File) loop

         declare
            Line : constant String := Trim (Ada.Text_IO.Get_Line (File), Side => Both);
            F    : Positive;
            L    : Natural;
            Include_token : constant String := "#include ";
            -- We are processing lines of that kind : 
            -- #include <stdio.h> 
            -- #include "square_root.h"
            -- The <> variant is used for system header files
            -- The "" variant is used for normal files of the program
            -- Cf. https://www.gnu.org/software/c-intro-and-ref/manual/html_node/include-Syntax.html
         begin
            Col := Index (Source  => Line,
                          Pattern => Include_token,
                          Going   => Forward,
                          Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
            if Col /= 0 then
               Col := Col + Include_token'length + 1;
               Find_Token (Source  => Line,
                           Set     => Ada.Strings.Maps.To_Set ("<>"""),
                           From    => Col,
                           Test    => Outside,
                           First   => F,
                           Last    => L);
               declare
                  Depends_On : constant String := Line (Col .. L - 2);
               
               begin
                  if Depends_On /= Unit_Name then -- to avoid x.c dependence on x.h
                     IO.Put_Line ("   - depends on >" & (Depends_On) & "<",
                                  Level => IO.Verbose);
   
                     Dep_List.Append ((To_Unit  => +Depends_On,
                                       Location => Current_Location));
                     -- Fixme: Only one unit name per import statement in C??
                  end if;
               end;

            end if;

         end;

      end loop Analysis;
 
      -- Unit Name and kind is deducted directly from file name
      if Extension = ".c" then
         Archicheck.Units.Add_Unit
           (Unit    => (Name           => +Unit_Name,
                        Lang           => Sources.C,
                        Kind           => Units.Implementation,
                        Implementation => True),
                        Targets        => Dep_List);

      elsif Extension = ".h" then
         Archicheck.Units.Add_Unit
           (Unit    => (Name           => +Unit_Name,
                        Lang           => Sources.C,
                        Kind           => Units.Interface_K,
                        Implementation => False),
                        Targets        => Dep_List);
      else raise WTF;
      end if;

   exception
      when Error : others => 
         IO.Put_Exception
            (Sources.Location_Image (Current_Location) & "parse exception:");
             IO.Put_Exception (Ada.Exceptions.Exception_Information (Error));
             Ada.Text_IO.Close (File => File);

   end Analyze_Dependencies;


end Archicheck.Lang.C_Processor;
