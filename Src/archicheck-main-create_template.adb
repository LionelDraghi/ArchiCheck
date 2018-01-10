-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (Archicheck.Main)

procedure Create_Template is
   Template : File_Type;

begin
   Create (Template);
   Set_Output (Template);

   New_Line;
   Put_Line ("-- Template archicheck file");
   Put_Line ("-- ------------------------");
   New_Line;
   
   Close (Template);
end Create_Template;
