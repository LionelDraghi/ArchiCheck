-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
--  Copyright (C) 1999 FlightSafety International and Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Bounded;

-------------------------------------------------------------------------------
--  This package hierarchy implements a mostly full-strength parser.
-------------------------------------------------------------------------------
package OpenToken is

   --  Exception raised by the analyzer when no match could be found.
   Syntax_Error : exception;

   --  Exception raised by the parser when no match could be found.
   Parse_Error : exception;

   --  We use this regardless of OS, since we need a standard way of
   --  representing an end of line in a string buffer
   EOL_Character : constant Character := Ada.Characters.Latin_1.CR;

   --  Similarly, this is independent of OS
   EOF_Character : constant Character := Ada.Characters.Latin_1.EOT;

   --  The maximum length of a token. Token.Analizer.Get_More_Text
   --  should return no more than this many characters minus the
   --  largest reasonable token string. Tune it up if it bugs you.
   Max_String_Length : constant := 1024;

   package Buffers is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_String_Length);

end OpenToken;
