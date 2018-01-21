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
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This package implements a token recognizer for a string of digits.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Extended_Digits is

   overriding procedure Clear (The_Token : in out Instance)
   is begin
      The_Token.State := First_Char;
   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is
      function Value_Is_In_Range return Boolean is
      begin
         case Next_Char is
         when 'A' .. 'F' => return Character'Pos (Next_Char) - Character'Pos ('A') + 10 < The_Token.Number_Base;
         when 'a' .. 'f' => return Character'Pos (Next_Char) - Character'Pos ('a') + 10 < The_Token.Number_Base;
         when '0' .. '9' => return Character'Pos (Next_Char) - Character'Pos ('0') +  0 < The_Token.Number_Base;
         when others   => return False;
         end case;
      end Value_Is_In_Range;

   begin

      case The_Token.State is

      when First_Char =>
         --  If the first character is a digit, it matches.

         if Value_Is_In_Range then
            Verdict         := Matches;
            The_Token.State := Extended_Digit;
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;

      when Extended_Digit =>
         --  If the character is a digit, it matches.
         --  If it is an underscore, so-far-so-good...

         if Value_Is_In_Range then
            Verdict := Matches;
         elsif Next_Char = '_' and The_Token.Allow_Underscores then
            Verdict         := So_Far_So_Good;
            The_Token.State := Underscore;
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;

      when Underscore =>
         --  If the character is a digit, it matches.

         if Value_Is_In_Range then
            Verdict         := Matches;
            The_Token.State := Extended_Digit;
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;

      when Done =>
         --  We shouldn't get called from here.

         Verdict := Failed;

      end case;

   end Analyze;

   ----------------------------------------------------------------------------
   --  This procedure will be called to create a Based Integer token
   ----------------------------------------------------------------------------
   function Get
     (For_Base          : Ada.Text_IO.Number_Base := 16;
      Allow_Underscores : Boolean                 := True;
      Reportable        : Boolean                 := False)
     return Instance
   is begin

      return (Report            => Reportable,
              Number_Base       => For_Base,
              Allow_Underscores => Allow_Underscores,
              State             => First_Char);

   end Get;

end OpenToken.Recognizer.Extended_Digits;
