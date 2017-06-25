-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Package: Archicheck.IO specification

with Ada.Text_IO;

package Archicheck.IO is

   procedure Put_Help;

   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String);
   procedure Put_Debug (Msg    : in String := "";
                        Debug  : in Boolean;
                        Prefix : in String);

   -- Mimics eponym Text_IO functions, except that if --quiet is set on command line, they have no effect
   procedure Put_Line (Item : String);
   procedure Put      (Item : String);
   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1);

   procedure Put_Warning (Msg       : in String := "");
   procedure Put_Error   (Msg       : in String := "";
                          With_Help : in Boolean := False);

end Archicheck.IO;
