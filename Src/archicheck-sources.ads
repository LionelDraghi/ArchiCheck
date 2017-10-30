-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.Sources specification
--

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Sources is

   -- --------------------------------------------------------------------------
   type Source is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Time_Tag : Ada.Calendar.Time;
   end record;
   package Source_Lists is new Ada.Containers.Doubly_Linked_Lists (Source);

   --**
   Source_List : Source_Lists.List := Source_Lists.Empty_List;

   -- -------------------------------------------------------------------------
   function Source_List_Image (Sources : in Archicheck.Sources.Source_Lists.List) return String;
   procedure Dump_Sources (Sources : in Archicheck.Sources.Source_Lists.List);


end Archicheck.Sources;
