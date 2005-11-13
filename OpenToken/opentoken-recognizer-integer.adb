-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2000 FlightSafety International and Ted Dennison
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
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- This software was originally developed by the following company, and was
-- released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-- Update History:
-- $Log: opentoken-recognizer-integer.adb,v $
-- Revision 1.3  2000/08/12 23:57:18  Ted
-- Changed some calls to dynamic dispatching to work around Gnat 3.13p bug
--
-- Revision 1.2  1999/12/27 19:56:02  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:35  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.3  1999/10/08 23:15:43  Ted
-- Add Get parameter to allow signed literals
--
-- Revision 1.2  1999/08/17 03:04:15  Ted
-- Add leading zero function
--
--
-- 1.3 - 27 June 1999  (C.K.W.Grein) Leading zero
-- 1.2 - 25 June 1999  (C.K.W.Grein) Use new Extended_Digits
-- 1.1 - 23 June 1999  (C.K.W.Grein) Amended functionality
-- 1.0                 Original version
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a token of an integer.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Integer is

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token: in out Instance) is
  begin

     -- Changed to dynamicly dispatch to work around gnat 3.13p bug
    Extended_Digits.Clear (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer));

    The_Token.Last_Verdict := Failed;
    The_Token.State        := First_Char;

  end Clear;

  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token: in out Instance;
                     Next_Char: in     Character;
                     Verdict  :    out Analysis_Verdict) is

    Decimal_Verdict: Analysis_Verdict := Failed;

  begin

    case The_Token.State is

      when First_Char =>

         case Next_Char is
            when '-' | '+' =>
               if The_Token.Allow_Signs then
                  The_Token.State   := First_Numeral;
               else
                  The_Token.State   := Done;
                  Verdict           := Failed;
               end if;

            when others =>
               -- Changed to dynamicly dispatch to work around gnat 3.13p bug
               Extended_Digits.Analyze
                 (The_Token => Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer),
                  Next_Char => Next_Char,
                  Verdict   => Decimal_Verdict);

               if Decimal_Verdict = Matches then
                  The_Token.Check_Zero := Next_Char = '0' and not The_Token.Allow_Leading_Zero;
                  Verdict         := Matches;
                  The_Token.State := Numeral;
               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;
         end case;

      when First_Numeral =>

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
         Extended_Digits.Analyze
           (The_Token => Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer),
            Next_Char => Next_Char,
            Verdict   => Decimal_Verdict);

         if Decimal_Verdict = Matches then
            The_Token.Check_Zero := Next_Char = '0' and not The_Token.Allow_Leading_Zero;
            Verdict         := Matches;
            The_Token.State := Numeral;
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;

      when Numeral =>
        -- If the numeral is a decimal integer, it matches.

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer),
                                 Next_Char, Decimal_Verdict);

        case Decimal_Verdict is
          when So_Far_So_Good |  -- Next_Char is '_'
               Matches =>        --           or decimal digit
            if The_Token.Check_Zero then  -- The string in question is not "0".
              Verdict         := Failed;
              The_Token.State := Done;
            else
              Verdict := Decimal_Verdict;
            end if;
          when Failed =>
            if The_Token.Last_Verdict = Matches and
               (Next_Char = 'e' or Next_Char = 'E') and The_Token.Allow_Exponent then
              Verdict         := So_Far_So_Good;
              The_Token.State := Exponent_Sign;
            else
              Verdict         := Failed;
              The_Token.State := Done;
            end if;
        end case;

      when Exponent_Sign =>
        -- If it is a digit, it matches.
        -- If it is a sign , so-far-so-good...

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Clear (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer));

        if Next_Char = '+' then

          Verdict         := So_Far_So_Good;
          The_Token.State := Exponent;

        else

           -- Changed to dynamicly dispatch to work around gnat 3.13p bug
          Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer),
                                   Next_Char, Decimal_Verdict);

          if Decimal_Verdict = Matches then  -- a decimal digit
            Verdict         := Matches;
            The_Token.State := Exponent;
          else
            Verdict         := Failed;
            The_Token.State := Done;
          end if;

        end if;

      when Exponent =>
        -- If the exponent is a decimal integer, it matches.

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer),
                                 Next_Char, Decimal_Verdict);

        case Decimal_Verdict is
          when So_Far_So_Good |   -- Next_Char is '_'
               Matches        =>  --           or decimal digit
            Verdict := Decimal_Verdict;
          when Failed =>
            Verdict         := Failed;
            The_Token.State := Done;
        end case;

      when Done =>

        -- We shouldn't get called from here.
        Verdict := Failed;

    end case;

    The_Token.Last_Verdict := Decimal_Verdict;

  end Analyze;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create an integer token
  ----------------------------------------------------------------------------
  function Get (Allow_Underscores  : Boolean := True;
                Allow_Exponent     : Boolean := True;
                Allow_Leading_Zero : Boolean := True;
                Allow_Signs        : Boolean := True) return Instance is
  begin

    return (Report             => True,
            Allow_Exponent     => Allow_Exponent,
            Allow_Leading_Zero => Allow_Leading_Zero,
            Allow_Signs        => Allow_Signs,
            Decimal_Recognizer => Extended_Digits.Get (Allow_Underscores => Allow_Underscores,
                                                       For_Base          => 10),
            Last_Verdict       => Failed,
            Check_Zero         => True,
            State              => First_Char);

  end Get;

end OpenToken.Recognizer.Integer;
