-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
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
--  This package implements a token recognizer for an Ada based teal literal.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Based_Real_Ada_Style is

   ----------------------------------------------------------------------------
   --  This procedure will be called when analysis on a new candidate string
   --  is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   overriding procedure Clear (The_Token : in out Instance) is
   begin

      The_Token.Number_Recognizer := Extended_Digits.Get
        (Allow_Underscores => True,
         For_Base          => 10);

      The_Token.Last_Verdict := Failed;
      The_Token.State        := Base;
      The_Token.Base         := 0;

   end Clear;

   ----------------------------------------------------------------------------
   --  This procedure will be called to perform further analysis on a token
   --  based on the given next character.
   ----------------------------------------------------------------------------
   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is
      Digits_Verdict : Analysis_Verdict := Failed;
   begin

      case The_Token.State is

         when Base =>
            --  If the base part is a decimal integer, so-far-so-good...

            Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |  -- Next_Char is '_'
                 Matches =>           --           or decimal digit

                  --  Keep a running total of the base. Verify that it isn't beyond the maximum.
                  if Next_Char /= '_' then
                     The_Token.Base := (The_Token.Base * 10) +
                       (Character'Pos (Next_Char) - Character'Pos ('0'));

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

                     --  Set up the subrecognizer for values of the specified base
                     The_Token.Number_Recognizer := Extended_Digits.Get
                       (Allow_Underscores => True,
                        For_Base          => The_Token.Base);
                     Verdict         := So_Far_So_Good;
                     The_Token.State := Fore;
                  else
                     Verdict         := Failed;
                     The_Token.State := Done;
                  end if;
            end case;

         when Fore =>
            --  If the fore consists of extended digits, so-far-so-good...
            --  If it is a '.', so-far-so-good...

            Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |   -- Next_Char is '_'
                 Matches           =>  --           or extended digit
                  Verdict := So_Far_So_Good;
               when Failed =>
                  if The_Token.Last_Verdict = Matches and Next_Char = '.' then
                     Verdict         := So_Far_So_Good;
                     The_Token.State := Aft;
                     Extended_Digits.Clear (The_Token.Number_Recognizer);
                  else
                     Verdict         := Failed;
                     The_Token.State := Done;
                  end if;
            end case;

         when Aft =>
            --  If the aft consists of extended digits, so-far-so-good...
            --  If it is a '#', it matches.

            Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

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
            --  If the character is an e/E, so-far-so-good...

            The_Token.Number_Recognizer := Extended_Digits.Get
              (Allow_Underscores => True,
               For_Base          => 10);

            if Next_Char = 'e' or Next_Char = 'E' then
               Verdict         := So_Far_So_Good;
               The_Token.State := Exponent_Sign;
               Extended_Digits.Clear (The_Token.Number_Recognizer);
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when Exponent_Sign =>
            --  If the first exponent character is a sign, so-far-so-good...
            --  If it is a decimal digit, it matches.

            if Next_Char = '+' or Next_Char = '-' then

               Verdict         := So_Far_So_Good;
               The_Token.State := Exponent;

            else

               Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

               if Digits_Verdict = Matches then  -- a decimal digit
                  Verdict         := Matches;
                  The_Token.State := Exponent;
               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;

            end if;

         when Exponent =>
            --  If the exponent is a decimal integer, it matches.

            Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

            case Digits_Verdict is
               when So_Far_So_Good |   -- Next_Char is '_'
                 Matches           =>  --           or decimal digit
                  Verdict := Digits_Verdict;
               when Failed =>
                  Verdict         := Failed;
                  The_Token.State := Done;
            end case;

         when Done =>
            --  We shouldn't get called from here.

            Verdict := Failed;

      end case;

      The_Token.Last_Verdict := Digits_Verdict;

   end Analyze;

   ----------------------------------------------------------------------------
   --  This procedure will be called to create a Based Integer token
   ----------------------------------------------------------------------------
   function Get return Instance is

   begin

      return (Report            => True,
              Number_Recognizer => Extended_Digits.Get (Allow_Underscores => True, For_Base => 10),
              Last_Verdict      => Failed,
              State             => Base,
              Base              => 0);

   end Get;

end OpenToken.Recognizer.Based_Real_Ada_Style;
