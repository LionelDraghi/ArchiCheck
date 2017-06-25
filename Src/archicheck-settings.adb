-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.Settings body

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Archicheck.Settings is

   Rules_Fl_Name : Unbounded_String := Null_Unbounded_String;

   -- -------------------------------------------------------------------------
   procedure Set_Rules_File_Name (Name : in String) is
   begin
      Rules_Fl_Name := To_Unbounded_String (Name);
   end Set_Rules_File_Name;

   -- -------------------------------------------------------------------------
   function Rules_File_Name return String is
   begin
     return To_String (Rules_Fl_Name);
   end Rules_File_Name;

   -- -------------------------------------------------------------------------
   function Src_Needed return Boolean is
   begin
      return List_Files or List_Dependencies;
   end Src_Needed;

   -- -------------------------------------------------------------------------
   function Rules_File_Needed return Boolean is
   begin
      return List_Components;
   end Rules_File_Needed;

end Archicheck.Settings;
