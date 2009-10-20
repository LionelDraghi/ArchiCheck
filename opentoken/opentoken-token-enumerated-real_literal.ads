-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003 Stephen Leake
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
--  This package declares a type for designating a real literal.
-------------------------------------------------------------------------------
generic
   type Real_Type is digits <>;
package OpenToken.Token.Enumerated.Real_Literal is

   type Instance is new OpenToken.Token.Enumerated.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Get a real literal token with the given ID and value.
   ----------------------------------------------------------------------------
   function Get (ID     : in Token_ID;
                 Value  : in Real_Type := 0.0
                ) return Instance'Class;

   ----------------------------------------------------------------------------
   --  This procedure will be called when a token is recognized.
   --
   --  The Token's ID will be set to the given value. The literal's value will
   --  be set to the real value of the Lexeme. The Recognizer field isn't
   --  used for this instance of the type.
   ----------------------------------------------------------------------------
   overriding procedure Create
     (Lexeme     : in     String;
      ID         : in     Token_ID;
      Recognizer : in     Recognizer_Handle;
      New_Token  :    out Instance);

   ----------------------------------------------------------------------------
   --  Return the value of the given token.
   ----------------------------------------------------------------------------
   function Value (Subject : in Instance) return Real_Type;

private
   type Instance is new OpenToken.Token.Enumerated.Instance with record
      Value : Real_Type;
   end record;

end OpenToken.Token.Enumerated.Real_Literal;
