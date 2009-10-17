-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


with Ada.Text_IO;

package body Archicheck.Source_Lists_IO is

   procedure Dump_Sources (Sources : in Source_Lists.List) is

      procedure Put_Source (Position : Source_Lists.Cursor) is
         use Ada.Strings.Unbounded;
      begin
         Ada.Text_IO.Put_Line (To_String (Source_Lists.Element (Position).Name));
      end Put_Source;

   begin
      Source_Lists.Iterate (Container => Sources,
                            Process   => Put_Source'Access);
   end Dump_Sources;

end Archicheck.Source_Lists_IO;
