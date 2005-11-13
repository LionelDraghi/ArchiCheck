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
-- $Log: opentoken-recognizer.ads,v $
-- Revision 1.1  1999/12/27 20:13:16  Ted
-- new parent for token recognizers
--
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Bounded;

-------------------------------------------------------------------------------
-- This package declares an abstract type for recognizing a single token. It
-- is designed to be called by an instance of the OpenToken.Analyzer class,
-- which will typically be calling several instances of Recognizer
-- simultaniously to find the best match for a string.
--
-- Note that the token analysis method of analyzing a character at a time
-- is a very fast method for complex grammars, but is also fairly complicated
-- to implement. Analyzers must be implemented as state-machines and
-- save their state between calls. This makes it a bit tough for meer mortals
-- to implement, so using the predefined tokens in the child packages is
-- highly reccomended where possible. State information should be saved in
-- the token itself to make the system reentrant.
-------------------------------------------------------------------------------
package OpenToken.Recognizer is

   -- Indication from a recognizer of how the given string matches.
   type Analysis_Verdict is
     (Matches,         -- The string matches
      So_Far_So_Good,  -- The string might match if it were just a bit longer
      Failed);         -- The string will never match, no matter how many more characters it has


   type Instance is abstract tagged record
      -- Indication of if this token is to be reported to the parser (client)
      -- when it is found. Set to FALSE to ignore tokens like comments and
      -- whitespace.
      Report : Boolean := True;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   -- This procedure will be called when analysis on a new candidate string
   -- is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   procedure Clear (The_Token : in out Instance) is abstract;


   ----------------------------------------------------------------------------
   -- This procedure will be called to perform further analysis on a token
   -- based on the given next character.
   ----------------------------------------------------------------------------
   procedure Analyze (The_Token : in out Instance;
                      Next_Char : in Character;
                      Verdict   : out Analysis_Verdict) is abstract;

end OpenToken.Recognizer;







