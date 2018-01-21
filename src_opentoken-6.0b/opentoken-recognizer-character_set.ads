-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
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
--
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a token of any
--  number of characters from a given set. The most useful use of this
--  facility in a typical application is locating strings of
--  "whitespace"
-----------------------------------------------------------------------------
package OpenToken.Recognizer.Character_Set is

   --  A typical definition set for whitespace. Use
   --  Ada.Strings.Maps.To_Set and Ada.Strings.Maps."&" create your
   --  own superset of this.
   Standard_Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.HT &
                                Ada.Characters.Latin_1.Space &
                                EOL_Character);

   type Instance is new OpenToken.Recognizer.Instance with private;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a Character_Set token.
   --  Set should be given a string containing all the characters that
   --  can be part of the token. The Set may be created using the
   --  operations in Ada.Strings.Maps. Putting EOL_Character in the
   --  set is perfectly acceptable, but EOF_Character is not a good
   --  idea.
   --
   --  It defaults to non-reportable because the typical use of this
   --  facility is for ignoring whitespace
   --------------------------------------------------------------------------
   function Get (Set        : in Ada.Strings.Maps.Character_Set;
                 Reportable : in Boolean := False) return Instance;

private


   type State_ID is (Text, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      --  The finite state machine state
      State : State_ID := Text;

      Set   : Ada.Strings.Maps.Character_Set;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Character_Set;
