-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
-- $Log: opentoken-recognizer-html_entity.ads,v $
-- Revision 1.1  1999/12/27 21:41:56  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/24  Grein
-- First Release
--
-- Revision 0.0  1999/12/16  Grein
-- Initial Version
-------------------------------------------------------------------------------

with Ada.Strings.Maps;

with OpenToken.Recognizer.Character_Set;

package OpenToken.Recognizer.HTML_Entity is

   -------------------------------------------------------------------------
   -- A recognizer for HTML entities like
   --   "&copy;"  named case-sensitive reference,
   --   "&#169;"  numeric decimal reference,
   --   "&#xA9;"  numeric hexadecimal reference case-insensitive,
   -- (all denoting the same character, the copyright sign).
   -- See the HTML 4.0 Reference for more information.
   -------------------------------------------------------------------------

   type Instance is new OpenToken.Recognizer.Instance with private;

   function Get return Instance;

private

   type State_Id is (Escape, First, Number, Rest, Done);

   type Instance is new OpenToken.Recognizer.Instance with record
      -- The finite state machine state
      State : State_Id := Escape;
      Set   : Ada.Strings.Maps.Character_Set;
   end record;

   procedure Clear (The_Token: in out Instance);

   procedure Analyze (The_Token: in out Instance;
                      Next_Char: in     Character;
                      Verdict  :    out OpenToken.Recognizer.Analysis_Verdict);

end OpenToken.Recognizer.HTML_Entity;
