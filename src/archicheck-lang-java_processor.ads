-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Java_Processor
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

   -- Here is a tutorial on Java packages :
   -- https://docs.oracle.com/javase/tutorial/java/package/packages.html

   type Java_Interface is new Lang_Interface with null record;

   -- --------------------------------------------------------------------------
   -- Procedure: Analyze_Dependencies
   -- Purpose:
   --   Analyze the source provided and add found dependencies to the list
   --
   -- Exceptions:
   --   Node_Already_Defined
   -- --------------------------------------------------------------------------
   overriding procedure Analyze_Dependencies
     (Lang        : in Java_Interface;
      From_Source : in Sources.File_Name);

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Lang : in Java_Interface) return String is ("*.java");

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize;


end Archicheck.Lang.Java_Processor;
