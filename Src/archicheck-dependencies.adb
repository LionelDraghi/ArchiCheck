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

package body Archicheck.Dependencies is

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
         Put_Line (Ada.Strings.Unbounded.To_String (D.From.Name) & " " & Unit_Description (D.From)
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
