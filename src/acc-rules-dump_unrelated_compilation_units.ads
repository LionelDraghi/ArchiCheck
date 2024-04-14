-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

procedure Acc.Rules.Dump_Unrelated_Compilation_Units;
-- This procedure will output compilation units (from sources), that
-- are not covered by some rules or component declaration in rules file.
-- This is intented to help users checking that the rules file (architecture
-- description) covers most (if not all) sources.
--
-- Result is a warning message for each Unit in the rule file that is neither
-- a component nor a compilation Unit.
