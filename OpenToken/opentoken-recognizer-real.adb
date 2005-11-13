-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
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
-- $Log: opentoken-recognizer-real.adb,v $
-- Revision 1.3  2000/08/12 23:57:18  Ted
-- Changed some calls to dynamic dispatching to work around Gnat 3.13p bug
--
-- Revision 1.2  1999/12/27 19:56:03  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:38  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.4  1999/10/22 02:24:20  Ted
-- Restructure Last_State assignments to avoid unitialized data problems
--
-- Revision 1.3  1999/10/08 23:15:43  Ted
-- Add Get parameter to allow signed literals
--
-- Revision 1.2  1999/08/17 02:53:55  Ted
-- Chris Grien fixes to the recognizer
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a token of a real literal.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Real is

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

    Decimal_Verdict: Analysis_Verdict;

  begin

    case The_Token.State is

      when First_Char =>

         case Next_Char is
            when '-' | '+' =>

               if The_Token.Allow_Signs then
                  Verdict         := So_Far_So_Good;
                  The_Token.State := First_Non_Sign;
                  Decimal_Verdict := Matches;

               else
                  Verdict := Failed;
                  The_Token.State := Done;
               end if;

            when '.' =>

               -- If the fore part is omitted and lazy reals are allowed, so-far-so-good...

               if The_Token.Allow_Laziness then
                  Verdict         := So_Far_So_Good;
                  The_Token.State := Aft_First;
               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;

            when others =>

               -- Changed to dynamicly dispatch to work around gnat 3.13p bug
               Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

               -- If the fore part is an integer, so-far-so-good...
               if Decimal_Verdict = Matches then
                  Verdict         := So_Far_So_Good;

                  The_Token.State        := Fore;
                  The_Token.Last_Verdict := Decimal_Verdict;
               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;

         end case;

      when First_Non_Sign =>
        -- If the fore part is omitted and lazy reals are allowed, so-far-so-good...
        -- If the fore part is an integer, so-far-so-good...

        if Next_Char = '.' then

          if The_Token.Allow_Laziness then
            Verdict         := So_Far_So_Good;
            The_Token.State := Aft_First;
          else
            Verdict         := Failed;
            The_Token.State := Done;
          end if;

        else

           -- Changed to dynamicly dispatch to work around gnat 3.13p bug
          Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

          if Decimal_Verdict = Matches then
            Verdict         := So_Far_So_Good;

            The_Token.State        := Fore;
            The_Token.Last_Verdict := Matches;
          else
            Verdict         := Failed;
            The_Token.State := Done;
          end if;

        end if;

      when Fore =>
        -- If the fore part is an integer, so-far-so-good...

        -- Note that any state that leads into this one needs to set
        -- The_Token.Last_Verdict to match the last verdict given from
        -- The_Token.Decimal_Recognizer.
         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

        case Decimal_Verdict is
          when So_Far_So_Good |  -- Next_Char is '_'
               Matches =>        --           or decimal digit
             Verdict := So_Far_So_Good;
             The_Token.Last_Verdict := Decimal_Verdict;
          when Failed =>
            if The_Token.Last_Verdict = Matches and Next_Char = '.' then
              if The_Token.Allow_Laziness then
                Verdict       := Matches;
              else
                Verdict       := So_Far_So_Good;
              end if;
              The_Token.State        := Aft;
              The_Token.Last_Verdict := Decimal_Verdict;

              -- Changed to dynamicly dispatch to work around gnat 3.13p bug
              Extended_Digits.Clear (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer));
            elsif The_Token.Last_Verdict = Matches     and
                  (Next_Char = 'e' or Next_Char = 'E') and
                  The_Token.Allow_Laziness and The_Token.Allow_Exponent then
              Verdict         := So_Far_So_Good;
              The_Token.State := Exponent_Sign;
              -- Changed to dynamicly dispatch to work around gnat 3.13p bug
              Extended_Digits.Clear (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer));
            else
              Verdict         := Failed;
              The_Token.State := Done;
            end if;
        end case;

      when Aft_First =>
        -- We arrive here only for omitted fore part.
        -- If the aft character is a digit, it matches.

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

        if Decimal_Verdict = Matches then
          Verdict                := Matches;
          The_Token.State        := Aft;
          The_Token.Last_Verdict := Matches;
        else
          Verdict         := Failed;
          The_Token.State := Done;
        end if;

      when Aft =>
        -- If the aft character is a digit it matches.
        -- If it is an underscore or an e/E, so-far-so-good...
        -- Note that any state that leads into this one needs to set
        -- The_Token.Last_Verdict to match the last verdict given from
        -- The_Token.Decimal_Recognizer.

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

        case Decimal_Verdict is
          when So_Far_So_Good =>  -- Next_Char is '_'
             Verdict := So_Far_So_Good;
             The_Token.Last_Verdict := Decimal_Verdict;
          when Matches =>         -- Next_Char is decimal digit
             Verdict := Matches;
             The_Token.Last_Verdict := Decimal_Verdict;
          when Failed =>
            if (The_Token.Last_Verdict = Matches or   -- Aft present
                (The_Token.Last_Verdict = Failed and  -- Aft not present (but Fore present)
                 The_Token.Allow_Laziness)) and
               (Next_Char = 'e' or Next_Char = 'E') and The_Token.Allow_Exponent then
              Verdict         := So_Far_So_Good;
              The_Token.State := Exponent_Sign;
              -- Changed to dynamicly dispatch to work around gnat 3.13p bug
              Extended_Digits.Clear (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer));
            else
              Verdict         := Failed;
              The_Token.State := Done;
            end if;
        end case;

      when Exponent_Sign =>
        -- If the first exponent character is a sign, so-far-so-good...
        -- If it is a decimal digit, it matches.

        if Next_Char = '+' or Next_Char = '-' then

          Verdict         := So_Far_So_Good;
          The_Token.State := Exponent;

        else

           -- Changed to dynamicly dispatch to work around gnat 3.13p bug
          Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

          if Decimal_Verdict = Matches then  -- a decimal digit
            Verdict         := Matches;
            The_Token.State := Exponent;
          else
            Verdict         := Failed;
            The_Token.State := Done;
          end if;

        end if;

      when Exponent =>
        -- If the exponent is a digit, it matches.
        -- If it is an underscore, so-far-so-good...

         -- Changed to dynamicly dispatch to work around gnat 3.13p bug
        Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Decimal_Recognizer), Next_Char, Decimal_Verdict);

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

  end Analyze;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create a Real token
  ----------------------------------------------------------------------------
  function Get (Allow_Underscores : Boolean := True;
                Allow_Exponent    : Boolean := True;
                Allow_Signs       : Boolean := True;
                Allow_Laziness    : Boolean := False) return Instance is

  begin

    return (Report             => True,
            Allow_Exponent     => Allow_Exponent,
            Allow_Laziness     => Allow_Laziness,
            Allow_Signs        => Allow_Signs,
            Decimal_Recognizer => Extended_Digits.Get (Allow_Underscores  => Allow_Underscores,
                                                       For_Base           => 10),
            Last_Verdict       => Failed,
            State              => First_Char);

  end Get;

end OpenToken.Recognizer.Real;
