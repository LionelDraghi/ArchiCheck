-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Ada
--
-- Purpose:
--   This package implement the abstract interface for Java sources.
--
-- Limitation:
--   Very Preliminary Implementation
--
-- Performance:
-- -----------------------------------------------------------------------------

package Archicheck.Lang.Java_Processor is


   type Java_Interface is new Lang_Interface with null record;

   -- --------------------------------------------------------------------------
   -- Procedure: Analyze_Dependencies
   -- Purpose:
   --   Analyze the source provided and add found dependencies to the list
   --
   -- Exceptions:
   --   Node_Already_Defined
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in Java_Interface;
                                   From_Source : in String);

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   function File_Extensions (Lang : in Java_Interface) return String;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize;


end Archicheck.Lang.Java_Processor;
