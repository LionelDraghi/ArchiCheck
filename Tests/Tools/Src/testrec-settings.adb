-- -----------------------------------------------------------------------------
-- Testrec, the Makefile test utility
-- Copyright (C) 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

package body Testrec.Settings is

   -- --------------------------------------------------------------------------
   function Log_File_Name return String is
   begin
      case Output_Format is
         when NaturalDocs => return "testrec.txt";
         when Markdown    => return "testrec.md";
      end case;
   end Log_File_Name;

end Testrec.Settings;
