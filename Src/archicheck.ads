-- Package: Archicheck specification

-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

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
   package Source_Lists is new Ada.Containers.Doubly_Linked_Lists (Source);

   type Dependency is record
      Unit_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Specification   : Boolean;
      Depends_On_Unit : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   package Dependency_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Dependency);

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
