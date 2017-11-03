-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


separate (Archicheck.Analyze_Rules_File)

function Is_Empty (Line : in String) return Boolean is
begin
   return Index_Non_Blank (Line) = 0;
end Is_Empty;
