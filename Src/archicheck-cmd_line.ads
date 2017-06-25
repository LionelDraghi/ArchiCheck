-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


-- Package: Archicheck.Cmd_Line specification

private package Archicheck.Cmd_Line is

   procedure Analyze_Cmd_Line (Line_OK : out Boolean);

--     function Source_List       return Source_Lists.List;
--     function Rules_File_Name   return String;
--     function List_Files        return Boolean;
--     function List_Dependencies return Boolean;
--     function List_Components   return Boolean;

end Archicheck.Cmd_Line;
