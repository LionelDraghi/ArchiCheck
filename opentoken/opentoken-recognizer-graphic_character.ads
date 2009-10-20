-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2008 Christoph Karl Walter Grein
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This package implements a token recognizer for a character literal 'x',
--  where all graphic characters are allowed for x except those given in an
--  exclusion set.
-------------------------------------------------------------------------------
with Ada.Strings.Maps;
package OpenToken.Recognizer.Graphic_Character is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   --  Call this procedure to create a Graphic_Character token.
   ----------------------------------------------------------------------------
   function Get (Exclude : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set)
                return Instance;

   ----------------------------------------------------------------------------
   --  Call this procedure to redefine the exclusion set.
   ----------------------------------------------------------------------------
   procedure Redefine (Inst    : in out Instance;
                       Exclude : in     Ada.Strings.Maps.Character_Set);

private

   type State_ID is (Opening_Tick, The_Character, Closing_Tick, Done);

   type Instance is new OpenToken.Recognizer.Instance with record
      Excluded : Ada.Strings.Maps.Character_Set;
      --  The finite state machine state
      State : State_ID := Opening_Tick;
   end record;

   ----------------------------------------------------------------------------
   --  This procedure will be called when analysis on a new candidate string
   --  is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   overriding procedure Clear (The_Token : in out Instance);

   ----------------------------------------------------------------------------
   --  This procedure will be called to perform further analysis on a token
   --  based on the given next character.
   ----------------------------------------------------------------------------
   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Graphic_Character;
