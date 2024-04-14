-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (Archicheck.Main)

-- --------------------------------------------------------------------------
procedure Put_Error (Msg       : in String  := "";
                     With_Help : in Boolean := False) is
begin
   IO.Put_Error (Msg);
   if With_Help then Put_Help; end if;
end Put_Error;
