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
--  This package implements a token recognizer for keywords. To match,
--  the token has to match the keyword EXACTLY. There is, however a
--  global switch for case-sensitivity.
-----------------------------------------------------------------------------
package OpenToken.Recognizer.Keyword is

   --  It is usually a lot easier on the user if keywords are not
   --  case-sensitive, so that's our default.
   Default_Case_Sensitivity : Boolean := False;

   type Instance is new OpenToken.Recognizer.Instance with private;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a keyword token.
   --  Setting Case_Sensitive to false allows the token to match
   --  keywords typed in upper or lower case.
   --------------------------------------------------------------------------
   function Get (Keyword_Literal : in String;
                 Case_Sensitive  : in Boolean := Default_Case_Sensitivity;
                 Reportable      : in Boolean := True) return Instance;

private


   type State_ID is (Text, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      --  The keyword definition
      Literal        : Buffers.Bounded_String;
      Case_Sensitive : Boolean;

      --  The finite state machine state
      State    : State_ID := Text;
      Substate : Integer  := 1;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Keyword;
