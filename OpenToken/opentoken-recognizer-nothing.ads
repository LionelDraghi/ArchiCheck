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
-- $Log: opentoken-recognizer-nothing.ads,v $
-- Revision 1.2  1999/12/27 19:56:03  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:37  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.1  1999/10/08 23:23:24  Ted
-- Recognizer that recognizes nothing
--
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer that recognizes absolutely
-- *Nothing*! Its intended use is for providing a good default token for
-- analyzers. But if you find another use for it, be my guest....
-------------------------------------------------------------------------------
package OpenToken.Recognizer.Nothing is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   -- This procedure will be called to create a nothing token.
   --
   -- If this token is being used as a default token, then setting reportable
   -- to "False" will allow you to completely ignore any syntax errors.
   ----------------------------------------------------------------------------
   function Get (Reportable : in Boolean := True) return Instance;

private


   type Instance is new OpenToken.Recognizer.Instance with null record;

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

end OpenToken.Recognizer.Nothing;

