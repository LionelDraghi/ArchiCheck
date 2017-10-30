-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck specification
-- Purpose:
--    This package contains some global declarations and data structures, like:
--    - Source_Lists
--    - Dependency_Lists
--    - Unit_Lists
--    - Component_Maps
--
--    Child units do the real job:
--
--    procedure <Archicheck> <Archicheck.Main body> - is in charge of controlling the execution
--    flow according to the command line
--    package <Archicheck.Cmd_Line> - do the command line analysis
--    package <Archicheck.Settings> - global settings, resulting mainly from cmd line analysis (and env. variables in the future)
--    Package <Archicheck.Rules_Parser> - encapsulates the rule file analysis
--    procedure <Archicheck.Check_*_Rules> - analyze the rules file and run the verifications
--    using OpenToken for rules file analisys
--    package <Archicheck.Sources> - Define a Source and manage the Source_List
--    package <Archicheck.Dependencies> - Define a Dependency and manage the Dependencies_List
--    package <Archicheck.Components> - Define a Component and the Unit_List it contains
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package Archicheck is

   --     -- --------------------------------------------------------------------------
   --     package Unit_Lists is new Ada.Containers.Doubly_Linked_Lists
   --       (Ada.Strings.Unbounded.Unbounded_String,
   --        Ada.Strings.Unbounded."=");
   --
   --     -- --------------------------------------------------------------------------
   --     package Component_Maps is new Ada.Containers.Indefinite_Hashed_Maps
   --       (Key_Type        => String,
   --        Element_Type    => Unit_Lists.List,
   --        Hash            => Ada.Strings.Hash,
   --        Equivalent_Keys => "=",
   --        "="             => Unit_Lists."=");
   --     Component_Map : Component_Maps.Map;

   -- --------------------------------------------------------------------------
   --** To be moved in a child pkg?
   type Layer_Relationship is record
      Using_Layer : Ada.Strings.Unbounded.Unbounded_String;
      Used_Layer  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   package Layer_Relationship_Lists is new Ada.Containers.Doubly_Linked_Lists (Layer_Relationship);
   Layers : Layer_Relationship_Lists.List;

end Archicheck;
