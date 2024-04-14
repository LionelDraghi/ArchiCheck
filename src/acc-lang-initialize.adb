-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Acc.Lang.Initialize body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Acc.Lang.Ada_Processor;
with Acc.Lang.C_Processor;
with Acc.Lang.Java_Processor;

procedure Acc.Lang.Initialize is
begin
   Ada_Processor.Initialize;
   C_Processor.Initialize;
   Java_Processor.Initialize;
end Acc.Lang.Initialize;
