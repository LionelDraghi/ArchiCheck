-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005-2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck specification
--
-- Purpose:
--    This package is empty. Child units do the real job :
--
--    procedure <Archicheck.Main> - is in charge of
--       controlling the execution flow according to the command line
--    package <Archicheck.Cmd_Line> - do the command line analysis
--    package <Archicheck.Settings> - global settings, resulting mainly from
--       cmd line analysis (and env. variables in the future)
--    package <Archicheck.Sources> - Defines a Source and manage the Source_List
--
-- Fixme: obsolete description!
--    Package <Archicheck.Rules_Parser> - encapsulates the rule file analysis
--    procedure <Archicheck.Check_*_Rules> - analyzes the rules file and run
--       the verifications using OpenToken for rules file analysis
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

package Archicheck is

   pragma Pure;

end Archicheck;
