--  Abstract :
--
--  See spec
--
--  Copyright (C) 2014-2015 Stephen Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);
package body OpenToken.Recognizer.Based_Integer_Real_Ada is

   overriding procedure Clear (Token : in out Instance)
   is begin
      --  Reset number_recognizer to decimal
      Token.Number_Recognizer := Extended_Digits.Get
        (Allow_Underscores => True,
         For_Base          => 10);

      Token.Last_Digits_Verdict := Failed;
      Token.State               := Base;
      Token.Base                := 0;
      Token.Need_Hash           := False;
   end Clear;

   overriding procedure Analyze
     (Token     : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is
      Digits_Verdict : Analysis_Verdict := Failed;
   begin
      case Token.State is
      when Base =>
         Extended_Digits.Analyze (Token.Number_Recognizer, Next_Char, Digits_Verdict);

         case Digits_Verdict is
         when Matches =>
            --  Next_Char is decimal digit

            Token.Base := (Token.Base * 10) + (Character'Pos (Next_Char) - Character'Pos ('0'));

            if Token.Base <= Extended_Digits.Maximum_Base then
               --  A based integer, decimal integer, or decimal real
               Verdict := Matches;
            else
               --  A decimal integer or decimal real
               Token.State := Fore;
               Verdict     := Matches;
            end if;

         when So_Far_So_Good =>
            --  Next_Char is '_'
            Verdict := So_Far_So_Good;

         when Failed =>
            --  Extended_Digits can fail for a trailing underscore, or for a non-digit.

            case Token.Last_Digits_Verdict is
            when Matches =>
               --  non-digit
               if Next_Char = '#' then
                  Token.Need_Hash := True;

                  --  use Base for the rest of the number
                  Token.Number_Recognizer := Extended_Digits.Get
                    (Allow_Underscores => True,
                     For_Base          => Token.Base);

                  Verdict     := So_Far_So_Good;
                  Token.State := Fore;

               else
                  Verdict     := Failed;
                  Token.State := Done;
               end if;

            when So_Far_So_Good =>
               --  trailing underscore in base
               Verdict     := Failed;
               Token.State := Done;

            when Failed =>
               --  First char in token; not a number
               Verdict     := Failed;
               Token.State := Done;

            end case;

         end case;

      when Fore =>
         Extended_Digits.Analyze (Token.Number_Recognizer, Next_Char, Digits_Verdict);

         case Digits_Verdict is
         when So_Far_So_Good | Matches =>
            --  integer or real. Extended_Digits returns So_Far_So_Good for underscore
            Verdict := Digits_Verdict;

         when Failed =>
            case Token.Last_Digits_Verdict is
            when Matches =>
               --  non-digit
               if Next_Char = '.' then
                  --  real number
                  Verdict     := So_Far_So_Good;
                  Token.State := Aft;
                  Extended_Digits.Clear (Token.Number_Recognizer);

               elsif Next_Char = '#' and Token.Need_Hash then
                  Verdict     := Matches;
                  Token.State := Exponent_E;

               else
                  Verdict     := Failed;
                  Token.State := Done;
               end if;

            when So_Far_So_Good =>
               --  Trailing underscore
               Verdict     := Failed;
               Token.State := Done;

            when Failed =>
               raise Programmer_Error with "Fore: State should be Done";
            end case;
         end case;

      when Aft =>
         Extended_Digits.Analyze (Token.Number_Recognizer, Next_Char, Digits_Verdict);

         case Digits_Verdict is
         when So_Far_So_Good | Matches =>
            Verdict := Digits_Verdict;

         when Failed =>
            case Token.Last_Digits_Verdict is
            when Matches =>
               --  Non-digit
               if Token.Need_Hash then
                  if Next_Char = '#' then
                     Verdict     := Matches;
                     Token.State := Exponent_E;
                  else
                     Verdict     := Failed;
                     Token.State := Done;
                  end if;

               elsif Next_Char = 'e' or Next_Char = 'E' then
                  Verdict     := So_Far_So_Good;
                  Token.State := Exponent_Sign;

                  --  Reset to decimal for exponent
                  Token.Number_Recognizer := Extended_Digits.Get
                    (Allow_Underscores => True,
                     For_Base          => 10);

               else
                  Verdict     := Failed;
                  Token.State := Done;
               end if;

            when So_Far_So_Good =>
               --  Trailing underscore
               Verdict     := Failed;
               Token.State := Done;

            when Failed =>
               --  First char in aft; not a real
               Verdict     := Failed;
               Token.State := Done;

            end case;
         end case;

      when Exponent_E =>

         if Next_Char = 'e' or Next_Char = 'E' then
            Verdict     := So_Far_So_Good; --
            Token.State := Exponent_Sign;

            --  Reset to decimal for exponent
            Token.Number_Recognizer := Extended_Digits.Get
              (Allow_Underscores => True,
               For_Base          => 10);

         else
            Verdict     := Failed;
            Token.State := Done;
         end if;

      when Exponent_Sign =>
         if Next_Char = '+' or Next_Char = '-' then
            Verdict     := So_Far_So_Good;
            Token.State := Exponent;

         else
            Extended_Digits.Analyze (Token.Number_Recognizer, Next_Char, Digits_Verdict);

            if Digits_Verdict = Matches then
               Verdict     := Matches;
               Token.State := Exponent;
            else
               Verdict     := Failed;
               Token.State := Done;
            end if;
         end if;

      when Exponent =>
         Extended_Digits.Analyze (Token.Number_Recognizer, Next_Char, Digits_Verdict);

         case Digits_Verdict is
         when So_Far_So_Good | Matches =>
            Verdict := Digits_Verdict;
         when Failed =>
            Verdict     := Failed;
            Token.State := Done;
         end case;

      when Done =>
         Verdict := Failed;

      end case;

      Token.Last_Digits_Verdict := Digits_Verdict;

   end Analyze;

   function Get return Instance
   is begin
      return
        (Report              => True,
         Number_Recognizer   => Extended_Digits.Get (Allow_Underscores => True, For_Base => 10),
         Last_Digits_Verdict => Failed,
         State               => Base,
         Base                => 0,
         Need_Hash           => False);
   end Get;

end OpenToken.Recognizer.Based_Integer_Real_Ada;
