-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck specification
--
-- This package contains some global declarations and data structures, like :
-- - Source_Lists
-- - Dependency_Lists
-- - Unit_Lists
-- - Component_Maps
--
-- Child units do the real job :
--
-- procedure <Archicheck> <Archicheck.Main body> - is in charge of controlling the execution flow according to the command line
-- package <Archicheck.Cmd_Line> - do the command line analysys
-- procedure <Archicheck.Analyze_Rules> - analyze the rules file and run the verifications
-- Procedure <Archicheck.Analyze_Rules_File> and childs - is the next generation Analyze_Rules, using OpenToken for rules file analisys. *Work in progress, not used in current code*
-- Function < Archicheck.Get_Dependencies> - is in charge of reading sources files, and populating the Dependency_Lists
-- package <Archicheck.Source_Lists_IO> - is in charge of dumping the Source_List, and will be in charge of loading such a list from a file when implemented

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Archicheck is

   type Source is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Time_Tag : Ada.Calendar.Time;
   end record;

   type Dependency is record
      Unit_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Specification   : Boolean;
      Depends_On_Unit : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Source_Lists     is new Ada.Containers.Doubly_Linked_Lists (Source);
   package Dependency_Lists is new Ada.Containers.Doubly_Linked_Lists (Dependency);

   package Unit_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   package Component_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Unit_Lists.List,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Unit_Lists."=");

end Archicheck;
