-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


separate (Archicheck.Analyze_Rules_File)

function Is_A_Component_Declaration (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " contains ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Component_Declaration;
