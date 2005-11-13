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
-- $Log: opentoken-recognizer-based_integer_ada_style.ads,v $
-- Revision 1.2  1999/12/27 19:55:59  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:31  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/10/22 04:31:49  Ted
-- Change instance to save value of base rather than its string representation
--
-- Revision 1.1  1999/08/17 03:07:26  Ted
-- Initial Version
--
--
-- 1.1 -  8 August 1999  Add check for base
-- 1.0 - 25 June   1999  First release
-------------------------------------------------------------------------------

with OpenToken.Recognizer.Extended_Digits;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for an Ada style based integer.
-- Such an integer consists of a base indication, followed by a sequence of
-- extended digits enclosed by '#', optionally followed by an exponent.
-- A base indication is a string of decimal digits with a value in the range
-- 2..16.
-- An extended digit is either a decimal digit or a character in the range
-- 'a'..'f' in any letter case. The extended digit must be smaller than the
-- base.
-- An exponent specification is a letter 'e' (or 'E') followed by an optional
-- plus sign and decimal digits.
-- Each of the sequences of digits may be interspersed with one or more single
-- underscores (but must not end with an underscore).
-------------------------------------------------------------------------------
package OpenToken.Recognizer.Based_Integer_Ada_Style is

  type Instance is new OpenToken.Recognizer.Instance with private;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create a Based Integer token.
  ----------------------------------------------------------------------------
  function Get return Instance;

private

  type State_ID is (Base, Numeral,
                    Exponent_E, Exponent_Sign, Exponent,
                    Done);

  type Instance is new OpenToken.Recognizer.Instance with record

    -- The finite state machine state
    Number_Recognizer: Extended_Digits.Instance;
    Last_Verdict     : Analysis_Verdict := Failed;
    State            : State_ID         := Base;
    Base             : Natural          := 0;

  end record;

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token: in out Instance);

  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token: in out Instance;
                     Next_Char: in     Character;
                     Verdict  :    out Analysis_Verdict);

end OpenToken.Recognizer.Based_Integer_Ada_Style;
