-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


-- Function: Archicheck.Get_Dependencies specification

private function Archicheck.Get_Dependencies
  (Source_Name  : String) return Archicheck.Dependency_Lists.List;
