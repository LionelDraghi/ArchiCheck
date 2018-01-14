-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

procedure Archicheck.Rules.Check_Unrelated_Compilation_Units;
-- This procedure will output units referenced in rules file that are
-- not related to some source.
-- This is intended to help users checking that there is no rules
-- silently failling with no effect because of some typo, or because of some
-- renaming in sources.
