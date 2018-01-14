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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;

private package Archicheck.Units is

   -- --------------------------------------------------------------------------
   type Unit_Name is new Unbounded_String;
   function "+" (Name : Unit_Name) return String;
   function "+" (Name : String) return Unit_Name;
   function To_US (Name : Unit_Name) return Unbounded_String;
   function "+" (Name : Unbounded_String) return Unit_Name;
   function "=" (X, Y : Unit_Name) return Boolean is
     (Ada.Strings.Equal_Case_Insensitive (+X, +Y));
   -- Redefines = as case insensitive.
   -- If a strict "=" is needed, "=" between Image could be used, or
   -- a Equal_Case_Sensitive function defined.
   -- But as it's to easy to use "=" in generic instantiation and cause a
   -- subtle bug, I choose this solution.
   Null_Unit_Name : constant Unit_Name := Unit_Name (Null_Unbounded_String);



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
                      Unknown); --  with Default_Value => Unknown;
   subtype Java_Unit_Kind      is Unit_Kind range Class_K .. Interface_K;
   subtype Ada_Unit_Kind       is Unit_Kind range Package_K .. Interface_K;
   subtype Ada_Subroutine_Kind is Unit_Kind range Procedure_K .. Function_K;
   subtype Compilation_Unit    is Unit_Kind range Package_K .. Interface_K;

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

   -- --------------------------------------------------------------------------
   type Dependency_Target is record
      To_Unit  : Unit_Name;
      -- File & Line : where the dependency comes from
      Location : Sources.Location;
   end record;

   package Dependency_Targets is new Ada.Containers.Doubly_Linked_Lists
     (Dependency_Target, "=");

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

   package Dependency_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Dependency, "=");

   Dependency_List : Dependency_Lists.List;

   -- --------------------------------------------------------------------------
   function Unit_Description (U : in Unit_Attributes) return String;
   -- Return a string describing the unit, depending on the language.
   -- It can be for exemple "package body" for Ada or "interface" for Java.

   -- --------------------------------------------------------------------------
   procedure Dump;
   -- Put all dependencies known at call time, one per line, in the following
   -- format :
   -- > P4 specification depends on P5

   -- --------------------------------------------------------------------------
   procedure Add_Unit (Unit    : Unit_Attributes;
                       Targets : Dependency_Targets.List);

   -- --------------------------------------------------------------------------
   procedure Add_Component (Component : Component_Attributes;
                            Targets   : Dependency_Targets.List);

   -- --------------------------------------------------------------------------
   function Is_A_Child (Child  : Unit_Name;
                        Parent : Unit_Name) return Boolean;

   -- --------------------------------------------------------------------------
   function Is_A_Component (Unit : Unit_Name) return Boolean;

   -- --------------------------------------------------------------------------
   function Is_In (Unit    : Unit_Name;
                   In_Unit : Unit_Name) return Boolean;
   -- return True if :
   -- - A is B
   -- - A is a child of B
   -- - A is contained by B, even indirectly
   -- Case Insensitive

   -- --------------------------------------------------------------------------
   function Is_Involved_In_A_Component (Unit : in Unit_Name) return Boolean;

   -- --------------------------------------------------------------------------
   function Hash_Case_Insensitive
     (Key : Unit_Name) return Ada.Containers.Hash_Type;

   -- The component map is a fast way to find what contains a Component.
   -- Note that it is maintained in parallel with the Dependency_List.
   package Component_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unit_Name,
      Element_Type    => Dependency_Targets.List,
      Hash            => Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => Dependency_Targets."=");
   function Get_Component_Map return Component_Maps.Map;

end Archicheck.Units;
