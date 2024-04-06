-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Ada
--
-- Purpose:
--   This package implement the abstract interface for Ada sources.
--
-- Limitation:
--
-- Performance:
-- -----------------------------------------------------------------------------

package Archicheck.Lang.Ada_Processor is


   type Ada_Interface is new Lang_Interface with null record;

   -- --------------------------------------------------------------------------
   -- Procedure: Analyze_Dependencies
   -- Purpose:
   --   Analyze the source provided and add found dependencies to the list
   --
   -- Exceptions:
   --   Node_Already_Defined
   -- --------------------------------------------------------------------------
   overriding procedure Analyze_Dependencies
     (Lang        : in Ada_Interface;
      From_Source : in Sources.File_Name);

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   overriding function File_Extensions 
     (Lang : in Ada_Interface) return String is ("*.ad[asb]");


   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize;


end Archicheck.Lang.Ada_Processor;
