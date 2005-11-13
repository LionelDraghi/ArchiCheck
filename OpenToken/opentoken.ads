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
-- $Log: opentoken.ads,v $
-- Revision 1.3  2000/01/27 21:07:34  Ted
-- Move the syntax_error exception here for greater visibility.
--
-- Revision 1.2  1999/12/27 19:56:04  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:40  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/08/17 02:47:28  Ted
-- Add log line
--
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Bounded;

-------------------------------------------------------------------------------
-- This package hierarchy implements a mostly full-strength parser.
-------------------------------------------------------------------------------
package OpenToken is

   -- Exception raised by the analyzer when no match could be found.
   Syntax_Error : exception;

   -- Set this to the text end-of-line indicator for this platform (DOS = CR, Unix = LF)
   EOL_Character : constant Character := Ada.Characters.Latin_1.CR;

   -- Set this to the end-of-file indicator for this platform/system.
   EOF_Character : constant Character := Ada.Characters.Latin_1.EOT;

   -- The maximum length of a token. Token.Analizer.Get_More_Text should return no more than
   -- this many characters minus the largest reasonable token string. Tune it up if it bugs
   -- you.
   Max_String_Length : constant := 1024;

private

   package Buffers is new Ada.Strings.Bounded.Generic_Bounded_Length(Max_String_Length);

end OpenToken;







