-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.Sources body

with Ada.Text_IO;

package body Archicheck.Sources is

   -- -------------------------------------------------------------------------
   function Source_List_Image (Sources : in Source_Lists.List) return String is
      use Ada.Strings.Unbounded;
      Tmp : Unbounded_String := Null_Unbounded_String;
   begin
      for Src of Sources loop
         Tmp := Tmp & Src.Name;
      end loop;
      return (To_String (Tmp));
   end Source_List_Image;

   -- -------------------------------------------------------------------------
   procedure Dump_Sources (Sources : in Source_Lists.List) is
      use Ada.Strings.Unbounded;

   begin
      for Src of Sources loop
         Ada.Text_IO.Put_Line (To_String (Src.Name));
      end loop;

   end Dump_Sources;

end Archicheck.Sources;
