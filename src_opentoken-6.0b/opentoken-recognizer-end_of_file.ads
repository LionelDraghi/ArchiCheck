-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
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
--
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package implements a token recognizer for an end of file.
--  This allows programs to detect the end of the file as normal
--  tokens without getting syntax errors.
-----------------------------------------------------------------------------
package OpenToken.Recognizer.End_Of_File is

   type Instance is new OpenToken.Recognizer.Instance with private;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a End_Of_File token. It
   --  defaults to reportable because doing otherwise would result in
   --  attempts to read more data, which would cause an error in the
   --  vast majority of cases.
   --------------------------------------------------------------------------
   function Get (Reportable : in Boolean := True) return Instance;

private


   type State_ID is (EOF, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      --  The finite state machine state
      State : State_ID := EOF;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.End_Of_File;
