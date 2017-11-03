-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Dependencies specification
-- Purpose:
--   This package manage the list of dependencies found while analyzing sources.
--
-- Effects:
--
-- Limitations:
--   Note that current implementation is limited to Ada files, and is limited
--   to package processing.
--   Dependencies in separate procedure are ignored.
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Dependencies is

   -- --------------------------------------------------------------------------
   type Dependency is record
      Unit_Name       : Unbounded_String;
      Specification   : Boolean;
      Depends_On_Unit : Unbounded_String;
   end record;
   package Dependency_Lists is new Ada.Containers.Doubly_Linked_Lists (Dependency);

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   -- Purpose:
   --   Analyze the source provided and add found dependencies to the list
   --
   -- Exceptions:
   --   Node_Already_Defined
   -- --------------------------------------------------------------------------
   procedure Add_Dependencies (From_Source : Unbounded_String);

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- Purpose:
   --   Returns the whole list of dependencies known at that time.
   --
   -- Exceptions:
   --   None
   -- --------------------------------------------------------------------------
   function Get_List return Dependency_Lists.List;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump
   -- Purpose:
   --   Put all dependencies known at call time, one per line, in the following
   --   format :
   --   > P4 specification depends on P5
   --
   -- Exceptions:
   --   None
   -- --------------------------------------------------------------------------
   procedure Dump;


end Archicheck.Dependencies;
