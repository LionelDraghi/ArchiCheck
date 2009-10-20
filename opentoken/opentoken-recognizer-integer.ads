-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
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
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with OpenToken.Recognizer.Extended_Digits;

---------------------------------------------------------------------------
--  This package implements a token recognizer for an integer. An
--  integer is defined as a string starting with a series of digits,
--  optionally followed by an exponent specification. An exponent
--  specification is a letter 'e' (or 'E') followed by an optional
--  plus sign and decimal digits. Each of the sequences of digits may
--  be interspersed with one or more single underscores (but must not
--  end with an underscore), if not explicitly dis- allowed. When this
--  recognizer and the one for Java style based integers are used
--  together in the same syntax, note that the integer recognizer must
--  be created with leading zero disallowed or a string like 07 is
--  ambiguous. (However 0 is always a simple integer.)
---------------------------------------------------------------------------
package OpenToken.Recognizer.Integer is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   --  This procedure will be called to create an Integer token.
   --
   --  One would probably want to disallow signs in languages that use
   --  '-' or '+' as an operator between integers. If your syntax does
   --  not have such a concept, you will probably want to allow them.
   --------------------------------------------------------------------------
   function Get (Allow_Underscores  : Boolean := True;
                 Allow_Exponent     : Boolean := True;
                 Allow_Leading_Zero : Boolean := True;
                 Allow_Signs        : Boolean := True
                ) return Instance;

private

   type State_ID is (First_Char, First_Numeral, Numeral, Exponent_Sign, Exponent, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      Allow_Exponent     : Boolean;
      Allow_Leading_Zero : Boolean;
      Allow_Signs        : Boolean;

      --  The finite state machine state
      Decimal_Recognizer : Extended_Digits.Instance;  -- Numeral, Exponent
      Last_Verdict       : Analysis_Verdict := Failed;
      Check_Zero         : Boolean          := True;
      State              : State_ID         := First_Char;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Integer;
