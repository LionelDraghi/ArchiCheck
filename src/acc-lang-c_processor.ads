-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Lang.C_Processor
--
-- Purpose:
--   This package implement the abstract interface for Java sources.
--
-- Limitation:
--   Very Preliminary Implementation
--
-- Performance:
-- -----------------------------------------------------------------------------

package Acc.Lang.C_Processor is

   type C_Interface is new Lang_Interface with null record;

   -- --------------------------------------------------------------------------
   -- Procedure: Analyze_Dependencies
   -- Purpose:
   --   Analyze the source provided and add found dependencies to the list
   --
   -- Exceptions:
   --   Node_Already_Defined
   -- --------------------------------------------------------------------------
   overriding procedure Analyze_Dependencies
     (Lang        : in C_Interface;
      From_Source : in Sources.File_Name);

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Lang : in C_Interface) return String is ("*.[ch]");

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize;


end Acc.Lang.C_Processor;
