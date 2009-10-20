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
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with OpenToken.Recognizer.Extended_Digits;

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a real number. An
--  real is defined as a string starting with a series of digits,
--  followed by a decimal point and another series of digits,
--  optionally followed by an exponent specification. An exponent
--  specification is a letter 'e' (or 'E') followed by an optional
--  sign and decimal digits. Each of the sequences of digits may be
--  interspersed with one or more single underscores (but must not end
--  with an underscore), if not explicitly dis- allowed. Optionally
--  lazy reals can be allowed, i.e. reals where one of the fore or aft
--  parts may be omitted and, when fore and the exponent are defined,
--  also the dot may be omitted. When allowing lazy real, be careful
--  not to allow integers with exponents, or else a literal like 1e+10
--  is ambiguous.
-----------------------------------------------------------------------------
package OpenToken.Recognizer.Real is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   --  This procedure will be called to create a Real token.
   --
   --  One would probably want to disallow signs in languages that use
   --  '-' or '+' as an operator between integers. If your syntax does
   --  not have such a concept, you will probably want to allow them.
   --------------------------------------------------------------------------
   function Get (Allow_Underscores : Boolean := True;
                 Allow_Exponent    : Boolean := True;
                 Allow_Signs       : Boolean := True;
                 Allow_Laziness    : Boolean := False) return Instance;

private

   type State_ID is (First_Char, First_Non_Sign, Fore, Aft_First, Aft, Exponent_Sign, Exponent, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      Allow_Exponent : Boolean := True;
      Allow_Laziness : Boolean := False;
      Allow_Signs    : Boolean := True;

      --  The finite state machine state
      Decimal_Recognizer : Extended_Digits.Instance;  -- Fore, Aft, Exponent
      Last_Verdict       : Analysis_Verdict := Failed;
      State              : State_ID         := First_Char;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure
     Analyze (The_Token : in out Instance;
              Next_Char : in     Character;
              Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Real;
