-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Rules specification
--
-- Purpose:
--    This package defines the three different possible rules in the rules
--    file, that is :
--    - the list of units wich use is always forbidden
--    - the list of units wich use is always allowed
--    - the list of possible relationship between two units
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Rules is

private
   subtype Unit_Name is Ada.Strings.Unbounded.Unbounded_String;

   -- --------------------------------------------------------------------------
   type Relationship_Kind is (Layer_Over, May_Use, Exclusive_Use);
   type Relationship is record
      Using_Unit : Unit_Name;
      Used_Unit  : Unit_Name;
      Kind       : Relationship_Kind;
   end record;

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- Procedure: Add_Relationship
   -- --------------------------------------------------------------------------
   -- --------------------------------------------------------------------------
   package Relationship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Relationship);
   function Get_List return Relationship_Lists.List;
   procedure Add_Relationship (R : in Relationship);

   -- --------------------------------------------------------------------------
   -- Function: Is_Forbidden
   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit : in Unit_Name) return Boolean;
   procedure Add_Forbidden_Unit (Unit : in Unit_Name);

   -- --------------------------------------------------------------------------
   -- Function: Is_Allowed
   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit : in Unit_Name) return Boolean;
   procedure Add_Allowed_Unit (Unit : in Unit_Name);

end Archicheck.Rules;
