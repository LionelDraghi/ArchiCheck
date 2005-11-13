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
-- $Log: opentoken-recognizer-separator.adb,v $
-- Revision 1.2  1999/12/27 19:56:03  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:39  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.1  1999/08/17 02:50:01  Ted
-- Initial Version
--
--
-- 1.1 - 23 June 1999  Wrong parameter name in Get corrected;
--                     corrected error in comment (reported by Ted Dennison)
-- 1.0 - 22 June 1999  First release
-------------------------------------------------------------------------------

package body OpenToken.Recognizer.Separator is

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token: in out Instance) is
  begin

    The_Token.State    := Text;
    The_Token.Substate := 1;

  end Clear;


  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token: in out Instance;
                     Next_Char: in     Character;
                     Verdict  :    out Analysis_Verdict) is

  begin

    case The_Token.State is
      when Text =>

        -- if the characters matches the current token character...
        if Next_Char = Buffers.Element(Source => The_Token.Literal,
                                       Index  => The_Token.Substate)
        then

          -- if its the last character, call it a match, otherwise
          -- so far so good.
          if The_Token.Substate = Buffers.Length (The_Token.Literal) then
            Verdict         := Matches;
            The_Token.State := Done;
          else
            Verdict            := So_Far_So_Good;
            The_Token.Substate := The_Token.Substate + 1;
          end if;

        else

          -- ...otherwise, it doesn't match
          Verdict         := Failed;
          The_Token.State := Done;

        end if;

      when Done =>

        -- We shouldn't get called from here.
        Verdict := Failed;

    end case;

  end Analyze;


  ----------------------------------------------------------------------------
  -- This procedure will be called to create a separator token
  ----------------------------------------------------------------------------
  function Get (Separator_Literal: in String;
                Reportable       : in Boolean := True) return Instance is

    New_Token : Instance;

  begin

    New_Token.Report  := Reportable;
    New_Token.Literal := Buffers.To_Bounded_String (Separator_Literal);

    return New_Token;

  end Get;

end OpenToken.Recognizer.Separator;
