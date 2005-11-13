-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: opentoken-token-enumerated-integer_literal.ads,v $
-- Revision 1.1  2000/08/12 13:54:40  Ted
-- moved from opentoken-token-integer_literal
--
-- Revision 1.1  2000/01/27 20:56:40  Ted
-- A token for integer literals.
--
-- Revision 1.1  1999/12/27 21:30:44  Ted
-- Initial Version
--
--
-------------------------------------------------------------------------------

with OpenToken.Recognizer;

-------------------------------------------------------------------------------
-- This package declares a type for designating an integer literal.
-------------------------------------------------------------------------------
generic
package OpenToken.Token.Enumerated.Integer_Literal is

   -- I'd prefer to use full named notation (OpenToken.Token.Enumerated.Instance)
   -- to refer to this type. But gnat has some wierd instatiation visibility bug
   -- with parent packages.
   subtype Enumerated_Instance is Instance;

   type Instance is new Enumerated_Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   -- Get an integer literal token with the given ID and value.
   ----------------------------------------------------------------------------
   function Get (ID     : in Token_ID;
                 Value  : in Integer := 0
                ) return Instance'Class;

   ----------------------------------------------------------------------------
   -- This procedure will be called when a token is recognized.
   --
   -- The Token's ID will be set to the given value. The literal's value will
   -- be set to the integer value of the Lexeme. The Recognizer filed isn't
   -- used for this instance of the type.
   ----------------------------------------------------------------------------
   procedure Create (Lexeme     : in     String;
                     ID         : in     Token_ID;
                     Recognizer : in     Recognizer_Handle;
                     New_Token  :    out Instance);

   ----------------------------------------------------------------------------
   -- Return the value of the given integer token.
   ----------------------------------------------------------------------------
   function Value (Subject : in Instance) return Integer;

private
   type Instance is new Enumerated_Instance with record
      Value : Integer;
   end record;

end OpenToken.Token.Enumerated.Integer_Literal;
