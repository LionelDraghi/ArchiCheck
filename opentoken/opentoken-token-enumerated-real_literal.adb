-------------------------------------------------------------------------------
--
-- Copyright (C) 2002 Stephen Leake
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

package body OpenToken.Token.Enumerated.Real_Literal is

   function Get (ID     : in Token_ID;
                 Value  : in Real_Type := 0.0) return Instance'Class is
   begin
      return Instance'Class (Instance'(ID => ID, Value => Value));
   end Get;

   overriding procedure
     Create (Lexeme     : in     String;
             ID         : in     Token_ID;
             Recognizer : in     Recognizer_Handle;
             New_Token  :    out Instance)
   is
      pragma Unreferenced (Recognizer);
   begin
      New_Token.ID := ID;
      New_Token.Value := Real_Type'Value (Lexeme);
   end Create;

   function Value (Subject : in Instance) return Real_Type is
   begin
      return Subject.Value;
   end Value;

end OpenToken.Token.Enumerated.Real_Literal;
