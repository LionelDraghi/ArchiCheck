-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Sources
--
-- Purpose:
--   This package defines Source, manage the Source list, and provides utilities
--   to print this list.
--
-- Effects:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Sources is

   -- -------------------------------------------------------------------------
   type Language is (Ada_2012, Java);

   -- --------------------------------------------------------------------------
   type Source is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      -- provision for "make like" analyzis : Time_Tag : Ada.Calendar.Time;
      Lang     : Language;
   end record;
   package Source_Lists is new Ada.Containers.Doubly_Linked_Lists (Source);

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Source_Lists.List;

   -- --------------------------------------------------------------------------
   -- Function: Add_Source
   -- --------------------------------------------------------------------------
   procedure Add_Source (Src : in Source);

   -- --------------------------------------------------------------------------
   -- Function: Add_List
   -- --------------------------------------------------------------------------
   procedure Add_List (List : in Source_Lists.List);

   -- -------------------------------------------------------------------------
   -- Function: Source_List_Image
   -- -------------------------------------------------------------------------
   function Source_List_Image (Sources : in Archicheck.Sources.Source_Lists.List) return String;

   -- -------------------------------------------------------------------------
   -- Procedure: Dump_Sources
   -- -------------------------------------------------------------------------
   procedure Dump_Sources (Sources : in Archicheck.Sources.Source_Lists.List);


end Archicheck.Sources;
