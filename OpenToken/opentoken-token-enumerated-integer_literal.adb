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
-- $Log: opentoken-token-enumerated-integer_literal.adb,v $
-- Revision 1.1  2000/08/12 13:54:40  Ted
-- moved from opentoken-token-integer_literal
--
-- Revision 1.1  2000/01/27 20:56:40  Ted
-- A token for integer literals.
--
--
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package declares a type for designating an integer literal.
-------------------------------------------------------------------------------
package body OpenToken.Token.Enumerated.Integer_Literal is

   ----------------------------------------------------------------------------
   -- Get a nonterminal token with the given ID.
   ----------------------------------------------------------------------------
   function Get (ID     : in Token_ID;
                 Value  : in Integer := 0) return Instance'Class is
   begin
      return Instance'Class(Instance'(ID => ID, Value => Value));
   end Get;

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
                     New_Token  :    out Instance) is
   begin
      New_Token.ID := ID;
      New_Token.Value := Integer'Value(Lexeme);
   end Create;

   ----------------------------------------------------------------------------
   -- Return the value of the given integer token.
   ----------------------------------------------------------------------------
   function Value (Subject : in Instance) return Integer is
   begin
      return Subject.Value;
   end Value;

end OpenToken.Token.Enumerated.Integer_Literal;
