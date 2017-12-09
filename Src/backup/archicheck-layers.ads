-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Layers specification
--
-- Purpose:
--    This package defines a dependency between two layers, and manage
--    the Layers_List.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Layers is

   type Layer_Relationship is record
      Using_Layer : Ada.Strings.Unbounded.Unbounded_String;
      Used_Layer  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Layer_Relationship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Layer_Relationship);

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Layer_Relationship_Lists.List;

   -- --------------------------------------------------------------------------
   -- Function: Add_Layer
   -- --------------------------------------------------------------------------
   procedure Add_Layer (Layer : in Layer_Relationship);

end Archicheck.Layers;
