-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
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
-- This software was originally developed by the following company, and was
-- released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-- Update History:
-- $Log: opentoken-recognizer-identifier.ads,v $
-- Revision 1.3  1999/12/27 21:27:39  Ted
-- Clarify comments
--
-- Revision 1.1  1999/12/27 17:11:35  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.3  1999/10/08 23:14:14  Ted
-- Add Get parameters to allow specification of allowable characters in the identifier.
--
-- Revision 1.2  1999/08/17 03:04:59  Ted
-- Add log line
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for an identifier. An identifier
-- is defined as a string starting with a character in a given set of start
-- characters and thereafter consisting of a sequence of characters in the
-- set of body characters.
-- Single underscores may be allowed inside identifiers.
-- When the underscore is in the set of characters, there is no
-- restriction on its use.
--
-- If you use an identifier token with keyword tokens, they will *both*
-- typically match. So be sure to list the keyword tokens in your syntax first,
-- so that the keywords will take precedence.
-------------------------------------------------------------------------------
package OpenToken.Recognizer.Identifier is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   -- This procedure will be called to create an Identifier token.
   --
   -- The defaults will give you an Ada-style identifier.
   -- For identifiers that allow multiple underscores in a row, add the
   -- underscore character to the Body_Chars. If it can also start with an
   -- underscore, add the underscore character to the Start_Chars.
   --
   -- Only set Has_Separator false when your identifier allows multiple
   -- underscores in a row.
   ----------------------------------------------------------------------------
   function Get (Start_Chars   : in Ada.Strings.Maps.Character_Set :=
                   Ada.Strings.Maps.Constants.Letter_Set;
                 Body_Chars    : in Ada.Strings.Maps.Character_Set :=
                   Ada.Strings.Maps.Constants.Alphanumeric_Set;
                 Has_Separator : in Boolean := True
                 ) return Instance;

private

   type State_ID is (First_Char, Text, Underscore, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      Start_Chars   : Ada.Strings.Maps.Character_Set;
      Body_Chars    : Ada.Strings.Maps.Character_Set;
      Has_Separator : Boolean;

      -- The finite state machine state
      State : State_ID := First_Char;

   end record;

   ----------------------------------------------------------------------------
   -- This procedure will be called when analysis on a new candidate string
   -- is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   procedure Clear (The_Token : in out Instance);

   ----------------------------------------------------------------------------
   -- This procedure will be called to perform further analysis on a token
   -- based on the given next character.
   ----------------------------------------------------------------------------
   procedure Analyze (The_Token : in out Instance;
                      Next_Char : in     Character;
                      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Identifier;
