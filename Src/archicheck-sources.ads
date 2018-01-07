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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Sources is

   -- --------------------------------------------------------------------------
   type Language is (Ada_2012, Java);

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String) return File_Name;
   function "+" (Name : File_Name) return Unbounded_String;
   -- function "+" (Name : Unbounded_String) return File_Name;

   -- --------------------------------------------------------------------------
   type Source is record
      File : File_Name;
      -- provision for "make like" analyzis : Time_Tag : Ada.Calendar.Time;
      Lang : Language;
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
   -- Procedure: Dump_Sources
   -- --------------------------------------------------------------------------
   procedure Dump_Sources (Sources : in Archicheck.Sources.Source_Lists.List);

   -- --------------------------------------------------------------------------
   -- Function: Location_Image
   --
   -- Purpose:
   --    This function return a source/line/column prefix to messages compatible
   --    whith GNU Standard
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --    That is :
   --       > sourcefile:lineno:
   --    if no column, or
   --       > sourcefile:lineno.column:
   --    otherwise.
   --
   --    Note that there is a trailing space, so that the message can be append
   --    directly.
   -- --------------------------------------------------------------------------
   function Location_Image (File   : in File_Name;
                            Line   : in Positive;
                            Column : in Integer := 0) return String;

   type Location is record
      File   : File_Name;
      Line   : Positive;
      Column : Integer := 0;
   end record;
   function Location_Image (Loc : in Location) return String;

end Archicheck.Sources;
