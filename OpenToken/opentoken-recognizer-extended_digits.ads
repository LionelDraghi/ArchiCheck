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
--
-- $Log: opentoken-recognizer-extended_digits.ads,v $
-- Revision 1.2  1999/12/27 19:56:01  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:34  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.3  1999/10/22 04:29:47  Ted
-- Add named-number for maximum supported base
--
-- Revision 1.2  1999/10/08 23:05:52  Ted
-- Fix comments
--
-- Revision 1.1  1999/08/17 03:05:35  Ted
-- Initial Version
--
-- 1.1 -  1 Sept 1999  Add comments
-- 1.0 - 25 June 1999  First release
-------------------------------------------------------------------------------

with Ada.Text_IO;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a sequence of extended
-- digits.
-- An extended digit is either a decimal digit or a character in the range
-- 'a'..'f' in any letter case. The extended digit must be smaller than the
-- base.
-- The sequence of digits may be interspersed with one or more single
-- underscores (but must not end with an underscore).
-- [For the OpenToken end user, this should not be too useful, but it shows up
-- handy for internal use.]
-------------------------------------------------------------------------------
package OpenToken.Recognizer.Extended_Digits is

  type Instance is new OpenToken.Recognizer.Instance with private;

  -- The maximum base value supported by this package
  Maximum_Base : constant := 16;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create an Extended Digits token for
  -- the given base.
  --
  -- It defaults to not reportable because it is mainly used as a building
  -- block for other recognizers.
  ----------------------------------------------------------------------------
  function Get (For_Base         : Ada.Text_IO.Number_Base := 16;
                Allow_Underscores: Boolean                 := True;
                Reportable       : Boolean                 := False)
               return Instance;

private

  type State_ID is (First_Char, Extended_Digit, Underscore, Done);

  type Instance is new OpenToken.Recognizer.Instance with record

    Number_Base      : Ada.Text_IO.Number_Base;
    Allow_Underscores: Boolean;

    -- The finite state machine state
    State: State_ID := First_Char;

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

end OpenToken.Recognizer.Extended_Digits;
