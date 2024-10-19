-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Sources
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Acc.IO;

with Ada.Strings.Fixed;


package body Acc.Sources is

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
   -- Procedure: Sort_And_Dump_Sources
   -- --------------------------------------------------------------------------
   procedure Sort_And_Dump_Sources is
      function "<" (L, R : Source) return Boolean is
      begin
         if L.Lang = R.Lang then
            return L.File < R.File;
         else
            return Language'Pos (L.Lang) < Language'Pos (R.Lang);
         end if;
      end "<";
      package Sorting is new Source_Lists.Generic_Sorting ("<");
      use Acc.IO;

   begin
      Sorting.Sort (Source_List);
      for Src of Source_List loop
         Put_Line (+Src.File, Level => Quiet);
      end loop;

   end Sort_And_Dump_Sources;

   -- --------------------------------------------------------------------------
   function Location_Image (Loc : in Location) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimed_File_Name : constant String := Trim (+Loc.File,
                                                  Side => Both);
      Trimed_Line      : constant String := Trim (Loc.Line'Image,
                                                  Side => Both);
      Trimed_Column    : constant String := Trim (Loc.Column'Image,
                                                  Side => Both);
   begin
      case Loc.Context is
         when In_File         =>
            if Loc.Column = 0 then
               return Trimed_File_Name & ":" & Trimed_Line & ": ";
            else
               return Trimed_File_Name & ":" & Trimed_Line & "." & Trimed_Column & ": ";
            end if;
         when In_Command_Line => return "Cmd line: ";
      end case;
   end Location_Image;

end Acc.Sources;
