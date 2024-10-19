-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Sources
--
-- Purpose:
--   This package defines Source, manage the Source list, and provides
--   utilities to print this list.
--
-- Effects:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

private package Acc.Sources is

   -- --------------------------------------------------------------------------
   type Language is (Ada_2012, C, Java);

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String) return File_Name;
   function "+" (Name : File_Name) return Unbounded_String;
   -- function "+" (Name : Unbounded_String) return File_Name;

   -- --------------------------------------------------------------------------
   type Parsing_Context is (In_File, In_Command_Line);
   type Location is record
      File    : File_Name;
      Context : Parsing_Context;
      Line    : Positive;
      Column  : Integer := 0;
   end record;

   function Location_Image (Loc : in Location) return String;
   -- --------------------------------------------------------------------------
   -- Purpose:
   --    This function return a source/line/column prefix to messages
   --    compatible with GNU Standard
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --    That is :
   --       > sourcefile:lineno:
   --    if no column, or
   --       > sourcefile:lineno.column:
   --    otherwise.
   --
   --    In the special case where the Lexer is parsing the command line,
   --    (Context set to Cmd_Line), the function return a constant
   --    "Cmd_Line :" string.
   --
   --    Note that there is a trailing space, so that the message can be
   --    appended directly.
   -- --------------------------------------------------------------------------

   -- --------------------------------------------------------------------------
   type Source is record
      File : File_Name;
      -- provision for "make like" analysis : Time_Tag : Ada.Calendar.Time;
      Lang : Language;
   end record;
   package Source_Lists is new Ada.Containers.Doubly_Linked_Lists (Source);

   -- --------------------------------------------------------------------------
   function Get_List return Source_Lists.List;
   procedure Add_Source (Src : in Source);
   procedure Sort_And_Dump_Sources;

end Acc.Sources;
