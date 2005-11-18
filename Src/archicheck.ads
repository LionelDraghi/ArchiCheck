with Ada.Calendar;
with Ada.Strings.Unbounded;

package Archicheck is

   type Source is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Time_Tag : Ada.Calendar.Time;
   end record;

   type Dependency is record
      Unit_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Depends_On_Unit : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Archicheck;
