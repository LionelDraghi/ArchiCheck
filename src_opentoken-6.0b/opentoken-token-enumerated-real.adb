-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2009, 2014 Stephen Leake
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

package body OpenToken.Token.Enumerated.Real is

   function Get
     (ID    : in Token_ID;
      Value : in Real_Type := 0.0;
      Name  : in String    := "";
      Build : in Action    := null)
     return Instance'Class
   is begin
      if Name = "" then
         return Instance'Class (Instance'(null, ID, Build, Value));
      else
         return Instance'Class (Instance'(new String'(Name), ID, Build, Value));
      end if;
   end Get;

   overriding procedure Create
     (Lexeme     : in     String;
      Bounds     : in     Buffer_Range;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance)
   is
      pragma Unreferenced (Bounds);
      pragma Unreferenced (Recognizer);
   begin
      New_Token.Value := Real_Type'Value (Lexeme);
   end Create;

   overriding procedure Copy
     (To   : in out Instance;
      From : in     Token.Class)
   is begin
      To.Value := Instance (From).Value;
   end Copy;

end OpenToken.Token.Enumerated.Real;
