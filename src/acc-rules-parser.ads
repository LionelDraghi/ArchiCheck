-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Rules.Parser specification
--
-- Purpose:
--   This package encapsulate the rules parser and the rules file grammar.
--   Short description, but big OpenToken mess inside.
--
-- Effects:
--
-- Performance:
-- -----------------------------------------------------------------------------

package Acc.Rules.Parser is

   -- -------------------------------------------------------------------------
   -- Procedure: Parse
   --
   -- Purpose:
   --    This procedure open and analyze the _File_Name_ rules file.
   --    Components description are stored in the Components package,
   --    Layers description in  Layers package, and so on.
   -- -------------------------------------------------------------------------
   procedure Parse (Rules_File : in Sources.File_Name);

end Acc.Rules.Parser;
