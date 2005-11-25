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

   package Unit_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   package Component_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Unit_Lists.List,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => Unit_Lists."=");

end Archicheck;
