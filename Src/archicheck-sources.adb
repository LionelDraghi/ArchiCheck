-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Sources
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Archicheck.IO;

with Ada.Strings.Fixed;


package body Archicheck.Sources is

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));
   -- function "+" (Name : Unbounded_String) return File_Name is
   --   (File_Name (Name));
   function "+" (Name : File_Name) return Unbounded_String is
     (Unbounded_String (Name));

   -- --------------------------------------------------------------------------
   Source_List : Source_Lists.List := Source_Lists.Empty_List;

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Source_Lists.List is (Source_List);

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Source
   -- --------------------------------------------------------------------------
   procedure Add_Source (Src : in Source) is
   begin
      Source_List.Append (Src);
   end Add_Source;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump_Sources
   -- --------------------------------------------------------------------------
   procedure Dump_Sources (Sources : in Source_Lists.List) is
      use Archicheck.IO;
   begin
      for Src of Sources loop
         Put_Line (+Src.File, Level => Quiet);
      end loop;
   end Dump_Sources;

   -- --------------------------------------------------------------------------
   function Location_Image (Loc : in Location) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimed_File   : constant String := Trim (+Loc.File, Side => Both);
      Trimed_Line   : constant String := Trim (Positive'Image (Loc.Line),
                                               Side => Both);
      Trimed_Column : constant String := Trim (Integer'Image (Loc.Column),
                                               Side => Both);
   begin
      if Loc.Column = 0 then
         return Trimed_File & ":" & Trimed_Line & ": ";
      else
         return Trimed_File & ":" & Trimed_Line & "." & Trimed_Column & ": ";
      end if;
   end Location_Image;

end Archicheck.Sources;
