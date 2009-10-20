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

with Ada.Strings.Maps.Constants;

package body OpenToken.Recognizer.HTML_Entity is

   function Get return Instance is
   begin
      return (Report => True,
              State  => Escape,
              Set    => Ada.Strings.Maps.Null_Set);
   end Get;

   overriding procedure Clear (The_Token : in out Instance) is
   begin
      The_Token.State := Escape;
      The_Token.Set   := Ada.Strings.Maps.Null_Set;
   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out OpenToken.Recognizer.Analysis_Verdict)
   is
      use type Ada.Strings.Maps.Character_Set;
   begin

      case The_Token.State is

         when Escape =>

            if Next_Char = '&' then
               Verdict         := So_Far_So_Good;
               The_Token.State := First;
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when First =>

            if Next_Char = '#' then
               Verdict         := So_Far_So_Good;
               The_Token.State := Number;
            elsif Ada.Strings.Maps.Is_In (Element => Next_Char,
                                          Set     => Ada.Strings.Maps.Constants.Letter_Set) then
               Verdict         := So_Far_So_Good;
               The_Token.State := Rest;
               The_Token.Set   := Ada.Strings.Maps.Constants.Letter_Set;
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when Number =>

            if The_Token.Set = Ada.Strings.Maps.Null_Set then
               if Next_Char = 'x' or Next_Char = 'X' then
                  Verdict         := So_Far_So_Good;
                  The_Token.Set   := Ada.Strings.Maps.Constants.Hexadecimal_Digit_Set;
                  --  We stay in this state so that we must have at least one digit.
               elsif Ada.Strings.Maps.Is_In (Element => Next_Char,
                                             Set     => Ada.Strings.Maps.Constants.Decimal_Digit_Set) then
                  Verdict         := So_Far_So_Good;
                  The_Token.State := Rest;
                  The_Token.Set   := Ada.Strings.Maps.Constants.Decimal_Digit_Set;
               else
                  Verdict         := Failed;
                  The_Token.State := Done;
               end if;
            elsif Ada.Strings.Maps.Is_In (Next_Char, The_Token.Set) then  -- this is hexadecimal
               Verdict         := So_Far_So_Good;
               The_Token.State := Rest;
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when Rest =>

            if Ada.Strings.Maps.Is_In (Next_Char, The_Token.Set) then
               Verdict         := So_Far_So_Good;
            elsif Next_Char = ';' then
               Verdict         := Matches;
               The_Token.State := Done;
            else
               Verdict         := Failed;
               The_Token.State := Done;
            end if;

         when Done =>

            Verdict := Failed;

      end case;

   end Analyze;

end OpenToken.Recognizer.HTML_Entity;
