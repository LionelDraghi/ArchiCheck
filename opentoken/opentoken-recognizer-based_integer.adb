-------------------------------------------------------------------------------
--
-- Copyright (C) 2006 Stephen Leake
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

package body OpenToken.Recognizer.Based_Integer is

   overriding procedure Clear (The_Token : in out Instance) is
   begin

      The_Token :=
        (Report            => True,
         Number_Recognizer => Extended_Digits.Get (Allow_Underscores => True, For_Base => 10),
         Last_Verdict      => Failed,
         State             => First_Char,
         Base              => 0,
         Based             => False);

   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is

      Digits_Verdict : Analysis_Verdict := Failed;

   begin

      case The_Token.State is
      when First_Char =>

         case Next_Char is
         when '-' | '+' =>
            The_Token.State := Maybe_Base;
            Verdict         := So_Far_So_Good;

         when others =>
            Extended_Digits.Analyze
              (The_Token => The_Token.Number_Recognizer,
               Next_Char => Next_Char,
               Verdict   => Digits_Verdict);

            if Digits_Verdict = Matches then
               Verdict         := Matches;
               The_Token.State := Maybe_Base;

               --  Keep a running total of the value, in case it is
               --  the base.

               The_Token.Base := (The_Token.Base * 10) + (Character'Pos (Next_Char) - Character'Pos ('0'));

            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;
         end case;

      when Maybe_Base =>

         Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

         case Digits_Verdict is
         when So_Far_So_Good =>
            --  Next_Char is '_'
            Verdict := So_Far_So_Good;

         when Matches =>
            --  Next_Char is a decimal digit

            The_Token.Base := (The_Token.Base * 10) + (Character'Pos (Next_Char) - Character'Pos ('0'));

            --  Test against maximum_base here, to avoid constraint
            --  error on large integers.

            Verdict := Matches;

            if The_Token.Base > Extended_Digits.Maximum_Base then
               The_Token.State := Numeral;
            end if;

         when Failed =>
            --  Can't have "16_#"
            if The_Token.Last_Verdict = Matches and Next_Char = '#' then

               The_Token.Based := True;

               --  Set up the subrecognizer for integers of the specified base
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

            Extended_Digits.Analyze (The_Token.Number_Recognizer, Next_Char, Digits_Verdict);

            case Digits_Verdict is
            when So_Far_So_Good =>
               --  Next_Char is '_'
               Verdict := So_Far_So_Good;

            when Matches =>
               --  Next_Char extended digit
               if The_Token.Based then
                  --  need trailing #
                  Verdict := So_Far_So_Good;
               else
                  Verdict := Matches;
               end if;

            when Failed =>
               if The_Token.Based then
                  if Next_Char = '#' then
                     if The_Token.Last_Verdict = Matches then
                        Verdict         := Matches;
                        The_Token.State := Done;
                     else
                        --  last char was trailing '_'
                        Verdict         := Failed;
                        The_Token.State := Done;
                     end if;
                  else
                     Verdict         := Failed;
                     The_Token.State := Done;
                  end if;

               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;
            end case;


         when Done =>
            --  We shouldn't get here.

            Verdict := Failed;

      end case;

      The_Token.Last_Verdict := Digits_Verdict;

   end Analyze;

   function Get return Instance
   is begin
      return
        (Report            => True,
         Number_Recognizer => Extended_Digits.Get (Allow_Underscores => True, For_Base => 10),
         Last_Verdict      => Failed,
         State             => First_Char,
         Base              => 0,
         Based             => False);
   end Get;

end OpenToken.Recognizer.Based_Integer;
