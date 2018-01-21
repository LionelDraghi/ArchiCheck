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
--
-----------------------------------------------------------------------------

with OpenToken.Recognizer.Extended_Digits;

--  This package implements a token recognizer for Ada integer
--  syntax, with optional base, no exponent, and optional preceding
--  sign.
--
--     [sign] base # based_numeral #
--     [sign] numeral

package OpenToken.Recognizer.Based_Integer is

   type Instance is new OpenToken.Recognizer.Instance with private;

   not overriding function Get return Instance;

private

   type State_ID is (First_Char, Maybe_Base, Numeral, Done);

   type Instance is new OpenToken.Recognizer.Instance with record
      Number_Recognizer : Extended_Digits.Instance;
      Last_Verdict      : Analysis_Verdict := Failed;
      State             : State_ID         := First_Char;
      Base              : Natural          := 0;
      Based             : Boolean          := False;
   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Based_Integer;
