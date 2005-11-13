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
-- $Log: opentoken-recognizer-graphic_character.ads,v $
-- Revision 1.2  1999/12/27 19:56:01  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:35  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/10/08 23:12:00  Ted
-- Add ability to exclude characters from the set
--
--
-- 1.1 -  4 July 1999  Exclusion set
-- 1.0 - 26 June 1999  First release
-------------------------------------------------------------------------------

with Ada.Strings.Maps;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a graphic character 'x',
-- where all graphic characters are allowed for x except those given in an
-- exclusion set.
-------------------------------------------------------------------------------
package OpenToken.Recognizer.Graphic_Character is

  type Instance is new OpenToken.Recognizer.Instance with private;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create a Graphic_Character token.
  ----------------------------------------------------------------------------
  function Get (Exclude : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set)
               return Instance;

private

  type State_ID is (Opening_Tick, The_Character, Closing_Tick, Done);

  type Instance is new OpenToken.Recognizer.Instance with record

    Excluded: Ada.Strings.Maps.Character_Set;

    -- The finite state machine state
    State: State_ID := Opening_Tick;

  end record;

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token : in out Instance);

  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token : in out Instance;
                     Next_Char : in     Character;
                     Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Graphic_Character;
