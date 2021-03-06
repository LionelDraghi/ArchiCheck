-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Archicheck.Lang.Initialize body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Archicheck.Lang.Ada_Processor;
with Archicheck.Lang.Java_Processor;

procedure Archicheck.Lang.Initialize is
begin
   Ada_Processor.Initialize;
   Java_Processor.Initialize;
end Archicheck.Lang.Initialize;
