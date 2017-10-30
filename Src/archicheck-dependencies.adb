-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Dependencies body
-- Implementation Notes:
--   -
--
-- Portability Issues:
--   None
--
-- Anticipated Changes:
--   - More more thorough Ada implementation needed
--   - Multi language pattern TBD
--
-- -----------------------------------------------------------------------------


with Archicheck.IO;
with Archicheck.Settings;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada_Lexer; use Ada_Lexer;

package body Archicheck.Dependencies is


   -- --------------------------------------------------------------------------
   Dependency_List : Dependency_Lists.List := Dependency_Lists.Empty_List;

   -- --------------------------------------------------------------------------
   -- Function: Get
   -- --------------------------------------------------------------------------
   function Get return Dependency_Lists.List is
   begin
      return Dependency_List;
   end Get;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   --
   -- Implementation Notes:
   --   - Based on OpenToken Ada_Lexer
   -- --------------------------------------------------------------------------
   procedure Add_Dependencies (From_Source : Unbounded_String) is

      -- Change default Debug parameter value to enable/disable Debug messages in this package
      -- -----------------------------------------------------------------------
      procedure Put_Debug_Line (Msg    : in String  := "";
                                Debug  : in Boolean := Settings.Debug_Mode;
                                Prefix : in String  := "Dependencies") renames Archicheck.IO.Put_Debug_Line;
      procedure Put_Debug (Msg    : in String  := "";
                           Debug  : in Boolean := Settings.Debug_Mode;
                           Prefix : in String  := "Dependencies") renames Archicheck.IO.Put_Debug;
      -- procedure New_Debug_Line (Debug  : in Boolean := Settings.Debug_Mode) renames Archicheck.IO.New_Debug_Line;

      -- Global text file for reading parse data
      File : Ada.Text_IO.File_Type;
      Tmp  : Dependency_Lists.List := Dependency_Lists.Empty_List;
      use Dependency_Lists;

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
      procedure Set_Unit_Name (List          : in out Dependency_Lists.List;
                               Unit_Name     : in String;
                               Specification : in Boolean) is
      begin
         for Dep of List loop
            Dep.Unit_Name     := To_Unbounded_String (Unit_Name);
            Dep.Specification := Specification;
         end loop;
      end Set_Unit_Name;

   begin
      -- New_Debug_Line;
      Put_Debug_Line ("Looking for dependencies in " & To_String (From_Source));

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => To_String (From_Source));
      Set_Input_Feeder (File);

      Source_Analysis : loop
         begin
            -- The withed units are first stored in
            -- a Tmp dependency list, with the Unit_Name left blank,
            -- When the package name is met, the Tmp list is modified to
            -- set the Unit_Name, then added to the returned dependency list.
            -- Limitation: only the fist Ada unit per source file is taken into account
            -- Limitation: only packages are taken into account

            Find_Next;
            case Token_ID is
            when With_T =>
               -- processing a "with"
               Find_Next;
               declare
                  Unit : constant String := Get_Unit_Name;
               begin
                  Put_Debug ("with on " &  Unit & " ");
                  Append (Tmp, (Unit_Name       => Null_Unbounded_String,
                                Depends_On_Unit => To_Unbounded_String (Unit),
                                Specification   => False));
               end;

            when Package_T =>
               -- processing the package declaration
               Find_Next;
               if Is_Empty (Tmp) then
                  -- we reach the package name without meeting any "with"
                  Put_Debug ("No with ");
               end if;

               if Token_ID = Body_T then
                  Find_Next;
                  declare
                     Unit : constant String := Get_Unit_Name;
                  begin
                     Put_Debug ("in package body " & Unit, Prefix => "");
                     Set_Unit_Name (Tmp, Unit, Specification => False);
                  end;

               else
                  declare
                     Unit : constant String := Get_Unit_Name;
                  begin
                     Put_Debug ("in package spec " & Unit, Prefix => "");
                     Set_Unit_Name (Tmp, Unit, Specification => True);
                  end;

               end if;
               for D of Tmp loop
                  Dependency_List.Prepend (D);
               -- let's reset the tmp list. This should be usefull only when processing
               -- a source embedding multiple package declaration, so that the "with" of
               -- the first pkg will not be attributed to following pkg.
               end loop;
               Clear (Tmp);

               --** optimization that prevent multiple pkg per file processing
               --** exit Source_Analysis;

               when others => null;

            end case;

            exit Source_Analysis when Token_ID = End_of_File_T;

         exception
            when Error : others =>
               IO.Put_Error
                 (IO.GNU_Prefix (To_String (From_Source), Line, Column) & "parse exception:");
               IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
         end;

      end loop Source_Analysis;

      Ada.Text_IO.Close (File => File);

   end Add_Dependencies;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump
   -- --------------------------------------------------------------------------
   procedure Dump is
      use Archicheck.IO;
   begin
      for D of Dependency_List loop
         Put (Ada.Strings.Unbounded.To_String (D.Unit_Name));
         if D.Specification then
            Put (" specification");
         else
            Put (" body         ");
         end if;
         Put_Line (" depends on " & To_String (D.Depends_On_Unit));
      end loop;
   end Dump;

end Archicheck.Dependencies;
