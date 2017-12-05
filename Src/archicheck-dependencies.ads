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
--   This package defines a Unit, and manage the list of dependencies found
--   while analyzing sources.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Archicheck.Sources;

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Dependencies is

   -- --------------------------------------------------------------------------
   type Unit_Kind is (Package_K,
                      Procedure_K,
                      Function_K,
                      Task_K,
                      Protected_K,
                      Class_K,
                      Interface_K,
                      Unknown) with Default_Value => Unknown;
   function Image (Kind : Unit_Kind) return String;

   type Unit is record
      Name           : Ada.Strings.Unbounded.Unbounded_String;
      File           : Ada.Strings.Unbounded.Unbounded_String;
      Lang           : Sources.Language;
      Kind           : Unit_Kind;
      Implementation : Boolean; -- False if Specification, or Interface,
                                -- True if implementation (or body, etc.)
   end record;

   -- --------------------------------------------------------------------------
   -- Function Unit_Description
   -- Purpose:
   --   Return a string describing the unit, depending on the language.
   --   It can be "package body" for Ada
   --   or "interface" for Java
   -- Exceptions:
   --   None
   -- --------------------------------------------------------------------------
   function Unit_Description (U : in Unit) return String;

   -- --------------------------------------------------------------------------
   type Dependency is record
      From : Unit;
      To   : Unit;
   end record;
   package Dependency_Lists is new Ada.Containers.Doubly_Linked_Lists (Dependency);

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- Purpose:
   --   Returns the whole list of dependencies known at that time.
   --
   -- Exceptions:
   --   None
   -- --------------------------------------------------------------------------
   function Get_List return Dependency_Lists.List;

   procedure Append (Dep : Dependency);

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
