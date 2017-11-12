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
         when Pkg_Spec       => return "specification";
         when Pkg_Body       => return "body";
         when Java_Class     => return "class";
         when Java_Interface => return "interface";
         when Unknown        => return "unknown";
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
   -- Procedure: Dump
   -- --------------------------------------------------------------------------
   procedure Dump is
      use Archicheck.IO;
   begin
      for D of Dependency_List loop
         Put_Line (Ada.Strings.Unbounded.To_String (D.From.Name) & " " & Image (D.From.Kind)
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
