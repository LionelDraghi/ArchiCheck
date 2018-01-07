-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang
--
-- Purpose:
--   This package defines the abstract interface that each child package
--   dedicated to a language have to implement, and services to identify
--   sources to be analyzed in a directory, and to analyze dependencies.
--
-- Effects:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Archicheck.Sources;

private package Archicheck.Lang is

   -- --------------------------------------------------------------------------
   -- procedure: Get_Src_List
   -- Purpose: return all sources in Root_Dir and surbdirs that is recognized
   -- by one of the langage processor.
   -- --------------------------------------------------------------------------
   procedure Get_Src_List (Root_Dir  : in String;
                           Recursive : in Boolean); --** to be renamed

   -- --------------------------------------------------------------------------
   -- procedure: Analyze_Dependencies
   -- Purpose: dispatch to to the right language anlyzer the processing of each
   --   identified source
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies;

   -- --------------------------------------------------------------------------
   -- Class: Lang_interface
   --
   --    Lang_Interfaces defines functions that should be implemented for each
   --    language, in child packages named Ada_Processor, Java_Processor, etc.
   --
   -- Procedure: Analyze_Dependencies
   --    Analyze the source provided and add found dependencies to the list
   -- Function:  File_Extensions
   --    Returns the regexp that will be used to identify sources files for the
   --    language.
   -- --------------------------------------------------------------------------
   type Lang_Interface is abstract tagged limited private;

   function File_Extensions
     (Lang : in Lang_Interface) return String is abstract;
   procedure Analyze_Dependencies
     (Lang        : in Lang_Interface;
      From_Source : in Sources.File_Name) is abstract;

private
   -- -------------------------------------------------------------------------
   type Lang_Interface is abstract tagged limited null record;

   type Interface_Access is access all Lang_Interface'class;

   -- -------------------------------------------------------------------------
   procedure Register (Language_Processor : in Interface_Access;
                       For_Language       : in Sources.Language);


end Archicheck.Lang;
