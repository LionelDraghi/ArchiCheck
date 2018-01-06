-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Units specification
-- Purpose:
--   This package defines a Unit and the Unit_List, and manage the list
--   of dependencies found while analyzing sources AND the rules files.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Archicheck.Sources;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

private package Archicheck.Units is

   -- --------------------------------------------------------------------------
   -- Units are either :
   -- 1. compilation units, common to more languages, like Packages, or
   --    specific to a language, like Protected records for Ada;
   -- 2. virtual units, hereafter "Components", that is units declared in rules
   --    files, thanks to the "contains" declaration.
   type Unit_Kind is (Package_K,
                      Procedure_K,
                      Function_K,
                      Task_K,
                      Protected_K,
                      Class_K,
                      Interface_K,
                      Component,
                      Unknown) with Default_Value => Unknown;

   -- --------------------------------------------------------------------------
   function Image (Kind : Unit_Kind) return String is
     (case Kind is
         when Package_K   =>  "package",
         when Procedure_K =>  "procedure",
         when Function_K  =>  "function",
         when Protected_K =>  "protected",
         when Task_K      =>  "task",
         when Class_K     =>  "class",
         when Interface_K =>  "interface",
         when Component   =>  "component",
         when Unknown     =>  "Unknown") with inline;

   subtype Unit_Name is Ada.Strings.Unbounded.Unbounded_String;

   type Dependency_Target is record
      To_Unit : Unit_Name;
      -- File & Line : where the dependency comes from
      File    : Ada.Strings.Unbounded.Unbounded_String;
      Line    : Natural;
   end record;
   -- --------------------------------------------------------------------------
   function Location_Image (Dep : Dependency_Target) return String;

   -- --------------------------------------------------------------------------
   package Dependency_Targets is new Ada.Containers.Doubly_Linked_Lists
     (Dependency_Target, "=");
   -- --------------------------------------------------------------------------
   function Unit_List_Image (List : Dependency_Targets.List) return String;

   -- --------------------------------------------------------------------------
   type Unit_Attributes (Kind : Unit_Kind := Unknown) is record
      Name : Unit_Name;
      case Kind is
         when Package_K .. Interface_K =>
            Lang           : Sources.Language;
            Implementation : Boolean;
            -- False if Specification, or Interface,
            -- True if implementation (or body, etc.)
         when Unknown | Component =>
            null;
      end case;
   end record;

   subtype Component_Attributes is Unit_Attributes (Kind => Component);

   -- --------------------------------------------------------------------------
   type Dependency is record
      Source  : Unit_Attributes;
      Targets : Dependency_Targets.List;
   end record;
   -- --------------------------------------------------------------------------
   package Dependency_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Dependency, "=");
   Dependency_List : Dependency_Lists.List;

   -- --------------------------------------------------------------------------
   -- Function Unit_Description
   -- Purpose:
   --   Return a string describing the unit, depending on the language.
   --   It can be "package body" for Ada
   --   or "interface" for Java
   -- Exceptions:
   --   None
   -- --------------------------------------------------------------------------
   function Unit_Description (U : in Unit_Attributes) return String;

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

   -- --------------------------------------------------------------------------
   procedure Add_Unit (Unit    : Unit_Attributes;
                       Targets : Dependency_Targets.List);

   -- --------------------------------------------------------------------------
   procedure Add_Component (Component : Component_Attributes;
                            Targets   : Dependency_Targets.List);

   -- --------------------------------------------------------------------------
   -- return True if :
   -- - A is B
   -- - A is a child of B
   -- - A is contained by B, even indirectly
   -- Case Insensitive
   function Is_In (Unit    : String;
                   In_Unit : String) return Boolean;

end Archicheck.Units;
