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
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This package implements a token recognizer for a line comment. A line comment
--  is a comment that runs from the introducer to the end of the line
-------------------------------------------------------------------------------
package body OpenToken.Recognizer.Line_Comment is

   ----------------------------------------------------------------------------
   --  This procedure will be called when analysis on a new candidate string
   --  is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   overriding procedure Clear (The_Token : in out Instance) is
   begin

      The_Token.State               := Introducer;
      The_Token.Introducer_Substate := 1;
   end Clear;


   ----------------------------------------------------------------------------
   --  This procedure will be called to perform further analysis on a token
   --  based on the given next character.
   ----------------------------------------------------------------------------
   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is begin

      case The_Token.State is
      when Introducer =>

         --  The character must match the comment introducer string
         if Next_Char = The_Token.Introducer_Text (The_Token.Introducer_Substate) then

            --  set the next state
            if The_Token.Introducer_Substate = The_Token.Introducer_Length then
               Verdict := Matches;
               The_Token.State := Text;
            else
               Verdict := So_Far_So_Good;
               The_Token.Introducer_Substate := The_Token.Introducer_Substate + 1;
            end if;

         else
            Verdict := Failed;
            The_Token.State := Done;
         end if;

      when Text =>

         --  If we read an end of line, its no longer a comment. Fail and go to done
         if Next_Char = EOL_Character or Next_Char = EOF_Character then
            Verdict := Failed;
            The_Token.State := Done;
         else
            Verdict := Matches;
         end if;

      when Done =>

         --  We shouldn't get called from here.
         Verdict := Failed;

      end case;

   end Analyze;


   ----------------------------------------------------------------------------
   --  This procedure will be called to create a Line Comment token
   ----------------------------------------------------------------------------
   function Get (Comment_Introducer : in String;
                 Reportable         : in Boolean := False) return Instance is

      New_Token : Instance;
   begin

      New_Token.Introducer_Text (1 .. Comment_Introducer'Length) := Comment_Introducer;
      New_Token.Introducer_Length := Comment_Introducer'Length;

      New_Token.Report := Reportable;

      return New_Token;

   end Get;

end OpenToken.Recognizer.Line_Comment;
