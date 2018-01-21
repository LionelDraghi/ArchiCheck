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
with Ada.Strings.Unbounded;

-------------------------------------------------------------------------------
--  This package provides a text feeder class that returns user-defined strings.
-------------------------------------------------------------------------------
package OpenToken.Text_Feeder.String is

   type Instance is new OpenToken.Text_Feeder.Instance with private;

   --------------------------------------------------------------------------
   --  This function returns strings for the analyzer. This version of
   --  it returns the string given it with the set command the first
   --  time it is called. On subsequent calls, Token.EOF_Character is
   --  retured.
   --------------------------------------------------------------------------
   overriding procedure Get
     (Feeder   : in out Instance;
      New_Text :    out Standard.String;
      Text_End :    out Integer);

   ----------------------------------------------------------------------------
   --  This function sets the string to be returned the next time Get is called
   ----------------------------------------------------------------------------
   procedure Set (Feeder : out Instance;
                  Value  : in  Standard.String
                 );

   --------------------------------------------------------------------------
   --  Return True if there is no more text to process. For this
   --  feeder that happens after a Get when no Set has yet been
   --  called.
   --------------------------------------------------------------------------
   overriding function End_Of_Text (Feeder : in Instance) return Boolean;


private
   type Instance is new OpenToken.Text_Feeder.Instance with record
      Next_Value : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;
end OpenToken.Text_Feeder.String;
