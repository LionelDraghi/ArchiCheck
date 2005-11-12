with Ada.Calendar;
with Ada.Strings.Unbounded;

package Archicheck is

   type Source is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Time_Tag : Ada.Calendar.Time;
   end record;

end Archicheck;
