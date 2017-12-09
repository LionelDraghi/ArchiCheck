-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Dependencies body
--
-- Implementation Notes:
--
-- Portability Issues:
--   None
--
-- Anticipated Changes:
--   - More more thorough Ada implementation needed
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

package body Archicheck.Dependencies is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False; -- change to True to debug this package
                             Prefix : in String  := "Dependencies") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   Dependency_List : Dependency_Lists.List := Dependency_Lists.Empty_List;

   -- --------------------------------------------------------------------------
   function Image (Kind : Unit_Kind) return String is
   begin
      case Kind is
         when Package_K   => return "package";
         when Procedure_K => return "procedure";
         when Function_K  => return "function";
         when Protected_K => return "protected";
         when Task_K      => return "task";
         when Class_K     => return "class";
         when Interface_K => return "interface";
         when Unknown     => return "Unknown";
      end case;
   end Image;

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Dependency_Lists.List is (Dependency_List);

   -- --------------------------------------------------------------------------
   -- Function: Is_Unit_In
   -- --------------------------------------------------------------------------
   function Is_Unit_In (Unit      : String;
                        Component : String) return Boolean
   is
      -- -----------------------------------------------------------------------
      -- Function: To_Lower
      -- -----------------------------------------------------------------------
      function To_Lower (Source  : String) return String is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Ada.Strings.Unbounded;
      begin
         return Translate (Source,
                           Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      end To_Lower;

      use Ada.Strings.Unbounded;
      use Ada.Strings;
      use Ada.Strings.Fixed;

   begin
      if To_Lower (Unit) = To_Lower (Component) then
         -- The Unit is the component
         Put_Debug_Line ("Unit " & Unit & " is (in) the component " & Component);
         return True;

      elsif Unit'Length > Component'Length and then
        (To_Lower (Head (Unit, Count => Component'Length)) = To_Lower (Component) and
             Unit (Component'Length + 1) = '.')
      then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("Unit " & Unit & " is (as child) in the component " & Component);
         return True;

      else
         Put_Debug_Line ("Unit " & Unit & " not in component " & Component, Prefix => "");
         return False;

      end if;
   end Is_Unit_In;

   -- --------------------------------------------------------------------------
   -- Procedure: Append
   -- --------------------------------------------------------------------------
   procedure Append (Dep : Dependency) is
      use Dependency_Lists;
   begin
      Append (Container => Dependency_List,
              New_Item  => Dep);
   end Append;

   -- --------------------------------------------------------------------------
   -- Function Unit_Description
   -- --------------------------------------------------------------------------
   function Unit_Description (U : in Unit) return String is
      use Archicheck.Sources;
   begin
      case U.Lang is
         when Java =>
            return Image (U.Kind);

         when Ada_2012 =>
            if U.Implementation then
               return Image (U.Kind) & " body";
            else
               return Image (U.Kind) & " spec";
            end if;
      end case;
   end Unit_Description;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump
   -- --------------------------------------------------------------------------
   procedure Dump is
      use Archicheck.IO;
   begin
      for D of Dependency_List loop
         Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (D.From.Name) & " " & Unit_Description (D.From)
                               & " depends on "
                               & Ada.Strings.Unbounded.To_String (D.To.Name) & " "); --  & Image (D.To.Kind));
                                                                                     -- gives file details only when verbose :
         Put_Line ("   " & Ada.Strings.Unbounded.To_String (D.From.File)
                   & " ->  "
                   & Ada.Strings.Unbounded.To_String (D.To.File),
                   Only_When_Verbose => True);
      end loop;
   end Dump;

end Archicheck.Dependencies;
