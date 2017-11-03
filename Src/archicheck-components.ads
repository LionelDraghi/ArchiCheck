-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Components specification
--
-- Purpose:
--    Define a Component, the Unit_List each Component contains, and the global
--    Component Map.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

private package Archicheck.Components is

   -- --------------------------------------------------------------------------
   package Unit_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   -- --------------------------------------------------------------------------
   package Component_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Unit_Lists.List,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Unit_Lists."=");
   Component_Map : Component_Maps.Map; --** à déplacer dans le body

   -- --------------------------------------------------------------------------
   -- Function: Unit_List_Image
   -- -------------------------------------------------------------------------
   function Unit_List_Image (UL : Unit_Lists.List) return String;

   -- --------------------------------------------------------------------------
   -- Function: Component_List_Image
   -- -------------------------------------------------------------------------
   function Component_List_Image return String;

end Archicheck.Components;
