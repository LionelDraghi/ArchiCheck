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
-- $Log: opentoken-recognizer-based_integer_ada_style.adb,v $
-- Revision 1.3  2000/08/12 23:57:18  Ted
-- Changed some calls to dynamic dispatching to work around Gnat 3.13p bug
--
-- Revision 1.2  1999/12/27 19:55:59  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:31  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.3  1999/11/20 17:50:24  Ted
-- Fix error when subtoken is reset
--
-- Revision 1.2  1999/10/22 04:33:41  Ted
-- Keep track of prospective base's value, rather than its characters. This
-- aviods Constraint_Errors on large (non-based) integer literals.
--
-- Revision 1.1  1999/08/17 03:07:26  Ted
-- Initial Version
--
--
-- 1.1 -  8 August 1999  Add check for base
-- 1.0 - 25 June   1999  First release
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer for an Ada based integer literal.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Based_Integer_Ada_Style is

   ----------------------------------------------------------------------------
   -- This procedure will be called when analysis on a new candidate string
   -- is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   procedure Clear (The_Token: in out Instance) is
   begin

      The_Token.Number_Recognizer :=
        Extended_Digits.Get (Allow_Underscores => True, For_Base => 10);
      The_Token.Last_Verdict := Failed;
      The_Token.State        := Base;
      The_Token.Base         := 0;

   end Clear;

   ----------------------------------------------------------------------------
   -- This procedure will be called to perform further analysis on a token
   -- based on the given next character.
   ----------------------------------------------------------------------------
   procedure Analyze (The_Token: in out Instance;
                      Next_Char: in     Character;
                      Verdict  :    out Analysis_Verdict) is

      Digits_Verdict: Analysis_Verdict := Failed;

   begin

      case The_Token.State is

         when Base =>
            -- If the base part is a decimal integer, so-far-so-good...

            -- Changed to dynamicly dispatch to work around gnat 3.13p bug
            Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Number_Recognizer), Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |  -- Next_Char is '_'
                 Matches =>           --           or decimal digit

                  -- Keep a running total of the base. Verify that it isn't beyond the maximum.
                  if Next_Char /= '_' then
                     The_Token.Base := (The_Token.Base * 10) +
                       (Character'Pos(Next_Char) - Character'Pos('0'));

                     if The_Token.Base <= Extended_Digits.Maximum_Base then
                        Verdict := So_Far_So_Good;
                     else
                        Verdict := Failed;
                     end if;
                  else
                     Verdict := So_Far_So_Good;
                  end if;

               when Failed =>
                  if The_Token.Last_Verdict = Matches and Next_Char = '#' then

                     -- Set up the subrecognizer for integers of the specified base
                     The_Token.Number_Recognizer := Extended_Digits.Get
                       (Allow_Underscores => True,
                        For_Base          => The_Token.Base);
                     Verdict         := So_Far_So_Good;
                     The_Token.State := Numeral;
                  else
                     Verdict         := Failed;
                     The_Token.State := Done;
                  end if;
            end case;

         when Numeral =>
            -- If the numeral consists of extended digits, so-far-so-good...
            -- If it is a '#', it matches.

            -- Changed to dynamicly dispatch to work around gnat 3.13p bug
            Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Number_Recognizer), Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |   -- Next_Char is '_'
                 Matches           =>  --           or extended digit
                  Verdict := So_Far_So_Good;
               when Failed =>
                  if The_Token.Last_Verdict = Matches and Next_Char = '#' then
                     Verdict         := Matches;
                     The_Token.State := Exponent_E;
                  else
                     Verdict         := Failed;
                     The_Token.State := Done;
                  end if;
            end case;

         when Exponent_E =>
            -- If the character is an e/E, so-far-so-good...

            The_Token.Number_Recognizer := Extended_Digits.Get
              (Allow_Underscores => True,
               For_Base          => 10);

            if Next_Char = 'e' or Next_Char = 'E' then
               Verdict         := So_Far_So_Good;
               The_Token.State := Exponent_Sign;
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when Exponent_Sign =>
            -- If the first exponent character is a sign, so-far-so-good...
            -- If it is a decimal digit, it matches.

            if Next_Char = '+' then

               Verdict         := So_Far_So_Good;
               The_Token.State := Exponent;

            else

               -- Changed to dynamicly dispatch to work around gnat 3.13p bug
               Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Number_Recognizer), Next_Char, Digits_Verdict);

               if Digits_Verdict = Matches then  -- a decimal digit
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
            Extended_Digits.Analyze (Extended_Digits.Instance'Class(The_Token.Number_Recognizer), Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |   -- Next_Char is '_'
                 Matches           =>  --           or decimal digit
                  Verdict := Digits_Verdict;
               when Failed =>
                  Verdict         := Failed;
                  The_Token.State := Done;
            end case;

         when Done =>
            -- We shouldn't get called from here.

            Verdict := Failed;

      end case;

      The_Token.Last_Verdict := Digits_Verdict;

   end Analyze;

   ----------------------------------------------------------------------------
   -- This procedure will be called to create a Based Integer token
   ----------------------------------------------------------------------------
   function Get return Instance is

   begin

      return (Report            => True,
              Number_Recognizer => Extended_Digits.Get (Allow_Underscores => True, For_Base => 10),
              Last_Verdict      => Failed,
              State             => Base,
              Base              => 0);

   end Get;

end OpenToken.Recognizer.Based_Integer_Ada_Style;
