-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2014 FlightSafety International and Ted Dennison
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
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling;

-------------------------------------------------------------------------------
--  This package implements a token recognizer for an End of File designator.
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Keyword is

   overriding procedure Clear (The_Token : in out Instance) is
   begin

      The_Token.State := Text;
      The_Token.Substate := 1;

   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is
      Converted_Char : Character;
   begin

      --  Convert the character to lower case if we aren't case sensitive
      if The_Token.Case_Sensitive then
         Converted_Char := Next_Char;
      else
         Converted_Char := Ada.Characters.Handling.To_Lower (Next_Char);
      end if;

      case The_Token.State is
      when Text =>

         --  if the characters matches the current token character...
         if Converted_Char = Buffers.Element (Source => The_Token.Literal,
                                              Index  => The_Token.Substate)
         then

            --  if its the last character, call it a match, otherwise
            --  so far so good.
            if The_Token.Substate = Buffers.Length (The_Token.Literal) then
               Verdict         := Matches;
               The_Token.State := Done;
            else
               Verdict            := So_Far_So_Good;
               The_Token.Substate := The_Token.Substate + 1;
            end if;

            --  ...otherwise, it doesn't match
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;

      when Done =>

         --  We shouldn't get called from here.
         Verdict := Failed;

      end case;

   end Analyze;

   function Get
     (Keyword_Literal : in String;
      Case_Sensitive  : in Boolean := Default_Case_Sensitivity;
      Reportable      : in Boolean := True)
     return Instance
   is
      New_Token : Instance;
   begin

      New_Token.Case_Sensitive := Case_Sensitive;
      New_Token.Report         := Reportable;

      if Case_Sensitive then
         New_Token.Literal := Buffers.To_Bounded_String (Keyword_Literal);
      else
         New_Token.Literal := Buffers.To_Bounded_String
           (Ada.Characters.Handling.To_Lower (Keyword_Literal));
      end if;

      return New_Token;
   end Get;

end OpenToken.Recognizer.Keyword;
