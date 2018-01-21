-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Archicheck.IO;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;

procedure Archicheck.Rules.Dump_Unrelated_Compilation_Units is
   -- --------------------------------------------------------------------------
   -- Fixme: Uggly solution to manage a unique item list
   function Hash_Case_Insensitive
     (Key : Unit_Name) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (+Key));
   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unit_Name,
      Element_Type    => Integer, -- not used
      Hash            => Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => "=");
   Unit_Map : Maps.Map;

   use Archicheck.IO;
   use Sources;

begin
   -- First, build a list of all Unit name to check
   for R of Dependency_List loop
      if not Unit_Map.Contains (R.Source.Name) then
         Unit_Map.Insert (R.Source.Name, 0);
      end if;
      for D of R.Targets loop
         if not Unit_Map.Contains (D.To_Unit) then
            Unit_Map.Insert (D.To_Unit, 0);
         end if;
      end loop;
   end loop;

   -- Then check each Unit against Rules and Components
   for C in Unit_Map.Iterate loop
      if not Is_Involved_In_A_Component (Maps.Key (C))
        and not Is_Involved_In_A_Rule (Maps.Key (C))
      then
         Put_Line ((+Maps.Key (C)), Level => Quiet);
      end if;
   end loop;

end Archicheck.Rules.Dump_Unrelated_Compilation_Units;
