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

with Ada.Text_IO;

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a sequence of
--  extended digits. An extended digit is either a decimal digit or a
--  character in the range 'a'..'f' in any letter case. The extended
--  digit must be smaller than the base. The sequence of digits may be
--  interspersed with one or more single underscores (but must not end
--  with an underscore). [For the OpenToken end user, this should not
--  be too useful, but it shows up handy for internal use.]
-----------------------------------------------------------------------------
package OpenToken.Recognizer.Extended_Digits is

   type Instance is new OpenToken.Recognizer.Instance with private;

   --  The maximum base value supported by this package
   Maximum_Base : constant := 16;

   --------------------------------------------------------------------------
   --  This procedure will be called to create an Extended Digits
   --  token for the given base.
   --
   --  It defaults to not reportable because it is mainly used as a
   --  building block for other recognizers.
   --------------------------------------------------------------------------
   function Get
     (For_Base          : Ada.Text_IO.Number_Base := 16;
      Allow_Underscores : Boolean                 := True;
      Reportable        : Boolean                 := False)
     return Instance;

private

   type State_ID is (First_Char, Extended_Digit, Underscore, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      Number_Base       : Ada.Text_IO.Number_Base;
      Allow_Underscores : Boolean;

      --  The finite state machine state
      State : State_ID := First_Char;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Extended_Digits;
