-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Settings body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Acc.Settings is

   Rules_Fl_Name : Unbounded_String := Null_Unbounded_String;

   -- -------------------------------------------------------------------------
   -- Procedure: Set_Rules_File_Name
   -- -------------------------------------------------------------------------
   procedure Set_Rules_File_Name (Name : in String) is
   begin
      Rules_Fl_Name := To_Unbounded_String (Name);
   end Set_Rules_File_Name;

   -- --------------------------------------------------------------------------
   -- Function: Rules_File_Name
   -- --------------------------------------------------------------------------
   function Rules_File_Name return String is (To_String (Rules_Fl_Name));

   -- --------------------------------------------------------------------------
   -- Function: Src_Needed
   -- -------------------------------------------------------------------------
   function Src_Needed return Boolean is (List_Files or List_Dependencies);

   -- --------------------------------------------------------------------------
   -- Function: Rules_File_Needed
   -- -------------------------------------------------------------------------
   function Rules_File_Needed return Boolean is (List_Rules);

end Acc.Settings;
