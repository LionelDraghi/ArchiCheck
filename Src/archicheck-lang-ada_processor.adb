-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Ada_Processor body
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
with Ada.Text_IO;
with Archicheck.Dependencies;

with Ada_Lexer;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

package body Archicheck.Lang.Ada_Processor is

   -- --------------------------------------------------------------------------
   Processor : aliased Ada_Interface;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Subscribe (Language_Processor => Processor'Access,
                 For_Language       => Sources.Ada_2012);
   end Initialize;

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   function File_Extensions (Lang : in Ada_Interface) return String is
      pragma Unreferenced (Lang);
   begin
      return Settings.Ada_Files_Pattern;
   end File_Extensions;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   --
   -- Implementation Notes:
   --   - Based on OpenToken Ada_Lexer
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in Ada_Interface;
                                   From_Source : in String) is
      pragma Unreferenced (Lang);

      -- Change default Debug parameter value to enable/disable Debug messages in this package
      -- -----------------------------------------------------------------------
      procedure Put_Debug_Line (Msg    : in String  := "";
                                Debug  : in Boolean := Settings.Debug_Mode;
                                Prefix : in String  := "Ada_Processor.Analyze_Dependencies") renames Archicheck.IO.Put_Debug_Line;
      procedure Put_Debug (Msg    : in String  := "";
                           Debug  : in Boolean := Settings.Debug_Mode;
                           Prefix : in String  := "Ada_Processor.Analyze_Dependencies") renames Archicheck.IO.Put_Debug;
      -- procedure New_Debug_Line (Debug  : in Boolean := Settings.Debug_Mode) renames Archicheck.IO.New_Debug_Line;

      -- Global text file for reading parse data
      File : Ada.Text_IO.File_Type;

      use Dependencies;
      use Dependencies.Dependency_Lists;
      Tmp  : Dependency_Lists.List := Dependency_Lists.Empty_List;

      use Ada.Strings.Unbounded;
      use Ada_Lexer;

      -- With_Found,
      Unit_Type_Identified : Boolean := False;

      -- -----------------------------------------------------------------------
      -- Procedure: Get_Unit_Name
      -- -----------------------------------------------------------------------
      function Get_Unit_Name return String is
         Name : Unbounded_String := Null_Unbounded_String;
      begin
         Name := To_Unbounded_String (Lexeme);
         loop
            Find_Next;
            if Token_ID = Dot_T then
               Find_Next;
               Name := Name & "." & Lexeme;
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
      procedure Set_Unit_Name (List      : in out Dependency_Lists.List;
                               Unit_Name : in     String;
                               Kind      : in     Unit_Kind) is
      begin
         for Dep of List loop
            Dep.From.Name := To_Unbounded_String (Unit_Name);
            Dep.From.Kind := Kind;
         end loop;
      end Set_Unit_Name;

   begin
      -- New_Debug_Line;
      IO.Put_Line ("Looking for dependencies in " & From_Source & " :", Only_When_Verbose => True);

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => From_Source);
      Set_Input_Feeder (File);

      Source_Analysis : loop
         begin
            -- The withed units are first stored in
            -- a Tmp dependency list, with the Unit_Name left blank,
            -- When the package name is met, the Tmp list is modified to
            -- set the Unit_Name, then added to the returned dependency list.
            -- Limitation: only the fist Ada unit per source file is taken into account
            -- Limitation: only packages are taken into account

            case Token_ID is
            when With_T =>
               -- processing a "with"
               Unit_List : loop
                  Find_Next;
                  declare
                     Unit : constant String := Get_Unit_Name;
                  begin
                     IO.Put_Line ("   - " & Unit, Only_When_Verbose => True);
                     Append (Tmp, (From => (Name => Null_Unbounded_String,
                                            File => To_Unbounded_String (From_Source),
                                            Lang => Sources.Ada_2012,
                                            Kind => Unknown),
                                   To   => (Name => To_Unbounded_String (Unit),
                                            File => Null_Unbounded_String,
                                            Lang => Sources.Ada_2012, --** pour l'instant
                                            Kind => Pkg_Spec)));
                     exit Unit_List when Token_ID /= Comma_T;
                     -- otherwise, loop to continue in the list of comma separated withed unit
                  end;
               end loop Unit_List;

            when Package_T =>
               -- processing the package declaration
               Find_Next;
               Put_Debug_Line ("5 :" &  Ada_Token'Image (Token_ID));
               if Is_Empty (Tmp) then
                  -- we reach the package name without meeting any "with"
                  Put_Debug ("No with ");
               end if;

               if Token_ID = Body_T then
                  Find_Next;
                  declare
                     Unit : constant String := Get_Unit_Name;
                  begin
                     Put_Debug_Line ("in package body " & Unit, Prefix => "");
                     Set_Unit_Name (Tmp, Unit, Kind => Pkg_Body);
                     Unit_Type_Identified := True;
                  end;

               else
                  declare
                     Unit : constant String := Get_Unit_Name;
                  begin
                     Put_Debug_Line ("in package spec " & Unit, Prefix => "");
                     Set_Unit_Name (Tmp, Unit, Kind => Pkg_Spec);
                     Unit_Type_Identified := True;
                  end;

               end if;

               for D of Tmp loop
                  Dependencies.Append (D);
               end loop;

               -- let's reset the tmp list. This should be usefull only when processing
               -- a source embedding multiple package declaration, so that the "with" of
               -- the first pkg will not be attributed to following pkg.
               Clear (Tmp);

               exit Source_Analysis;
               -- This optimization (exiting before end of file) prevents multiple pkg per file processing.
               -- On the other hand, GtkAda analysis drop from 8s to 0.7s when uncommenting this line.
               -- Chechekd with :
               -- > time ../../Obj/archicheck -lf -I gtkada

               when others => Find_Next;

            end case;

            exit Source_Analysis when Token_ID = End_of_File_T;

         exception
            when Error : others =>
               IO.Put_Error
                 (IO.GNU_Prefix (From_Source, Line, Column) & "parse exception:");
               IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
         end;

      end loop Source_Analysis;

      Ada.Text_IO.Close (File => File);

      if not Unit_Type_Identified then IO.Put_Warning ("Unknown Ada Unit in " & From_Source); end if;

   end Analyze_Dependencies;

end Archicheck.Lang.Ada_Processor;
