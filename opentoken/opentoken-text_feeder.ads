-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
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

-----------------------------------------------------------------------------
--  This package provides an abstract text feeder class for use with
--  the token analyzer package.
-----------------------------------------------------------------------------
package OpenToken.Text_Feeder is

   type Instance is abstract tagged null record;

   ----------------------------------------------------------------------------
   --  This procedure returns strings for the analyzer.
   --
   --  Text_End should be set to the index of the last valid character
   --  in New_Text. (If New_Text /= 1 then this will *not* be the same
   --  as the length of the string!!)
   --
   --  Also keep these important facts in mind when writing your own
   --  version of this routine:
   --
   --  o  New_Text may be using any number for its 'First and 'Last attributes.
   --     Don't assume New_Text'First is 1!
   --  o  New_Text'length will be > 0, but may be as small as 1.
   --  o  If the end of the text is reached, a Token.EOF_Character should be
   --     retured.
   --
   ----------------------------------------------------------------------------
   procedure Get (Feeder   : in out Instance;
                  New_Text :    out String;
                  Text_End :    out Integer) is abstract;

   ----------------------------------------------------------------------------
   --  Return True if there is no more text to process.
   ----------------------------------------------------------------------------
   function End_Of_Text (Feeder : Instance) return Boolean is abstract;

end OpenToken.Text_Feeder;

