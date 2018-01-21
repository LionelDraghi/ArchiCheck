-------------------------------------------------------------------------------
--
-- Copyright (C) 2010, 2012, 2014 Stephen Leake
-- Copyright (C) 1999 Christoph Karl Walter Grein
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
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a bracketed
--  comment. A bracketed comment is a comment that runs from the
--  opener to the closer, optionally enclosing several lines.
-----------------------------------------------------------------------------
package body OpenToken.Recognizer.Bracketed_Comment is

   overriding procedure Clear (The_Token : in out Instance) is
   begin

      The_Token.State         := Opener;
      The_Token.Bracket_State := 1;
      The_Token.Nested_Depth  := 0;

   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is
      use Ada.Strings.Unbounded;
   begin

      case The_Token.State is

      when Opener =>
         --  The character must match the comment opener string

         if Next_Char = The_Token.Opener_Text (The_Token.Bracket_State) then

            Verdict := So_Far_So_Good;

            if The_Token.Bracket_State = The_Token.Opener_Length then
               The_Token.State := Text;
            else
               The_Token.Bracket_State := The_Token.Bracket_State + 1;
            end if;

         else

            Verdict         := Failed;
            The_Token.State := Done;

         end if;

      when Nest_Opener =>

         if Next_Char = The_Token.Opener_Text (The_Token.Bracket_State) then

            if The_Token.Bracket_State = The_Token.Opener_Length then
               The_Token.Nested_Depth := The_Token.Nested_Depth + 1;
               Verdict         := So_Far_So_Good;
               The_Token.State := Text;
            else
               Verdict                 := So_Far_So_Good;
               The_Token.Bracket_State := The_Token.Bracket_State + 1;
            end if;

            if The_Token.Report then
               The_Token.Text := The_Token.Text & Next_Char;
            end if;

         else

            Verdict         := So_Far_So_Good;
            The_Token.State := Text;

         end if;

      when Text =>
         --  When we hit the first character of the closer, it might
         --  be the end of the comment.

         Verdict := So_Far_So_Good;

         if Next_Char = The_Token.Closer_Text (1) then
            if The_Token.Nested then
               if The_Token.Closer_Length = 1 then
                  The_Token.Nested_Depth := The_Token.Nested_Depth - 1;
               else
                  The_Token.State         := Nest_Closer;
                  The_Token.Bracket_State := 2;
               end if;
            else
               if The_Token.Closer_Length = 1 then
                  Verdict         := Matches;
                  The_Token.State := Done;
               else
                  The_Token.State         := Closer;
                  The_Token.Bracket_State := 2;
               end if;
            end if;
         elsif The_Token.Nested and then Next_Char = The_Token.Opener_Text (1) then
            The_Token.State         := Nest_Opener;
            The_Token.Bracket_State := 2;
         end if;

      when Nest_Closer =>

         if Next_Char = The_Token.Closer_Text (The_Token.Bracket_State) then

            if The_Token.Bracket_State = The_Token.Closer_Length then
               The_Token.Nested_Depth := The_Token.Nested_Depth - 1;
               Verdict         := So_Far_So_Good;
               The_Token.State := Text;
            else
               Verdict                 := So_Far_So_Good;
               The_Token.Bracket_State := The_Token.Bracket_State + 1;
            end if;

         elsif Next_Char = The_Token.Closer_Text (1) then

            The_Token.Bracket_State := 2;
            Verdict                 := So_Far_So_Good;

         else

            Verdict         := So_Far_So_Good;
            The_Token.State := Text;

         end if;

      when Closer =>

         if Next_Char = The_Token.Closer_Text (The_Token.Bracket_State) then

            if The_Token.Bracket_State = The_Token.Closer_Length then
               Verdict         := Matches;
               The_Token.State := Done;
            else
               Verdict                 := So_Far_So_Good;
               The_Token.Bracket_State := The_Token.Bracket_State + 1;
            end if;

         elsif Next_Char = The_Token.Closer_Text (1) then

            Verdict                 := So_Far_So_Good;
            The_Token.State         := Closer;
            The_Token.Bracket_State := 2;

         else

            Verdict         := So_Far_So_Good;
            The_Token.State := Text;

         end if;

      when Done =>

         --  We shouldn't get called from here.
         Verdict := Failed;

      end case;

   end Analyze;

   function Get
     (Comment_Opener : in String;
      Comment_Closer : in String;
      Reportable     : in Boolean := False;
      Nested         : in Boolean := False)
     return Instance
   is
   begin

      return
        (Report        => Reportable,
         Nested        => Nested,
         Opener_Text   => Comment_Opener & (Comment_Opener'Length + 1 .. Max_Bracket_Length => ' '),
         Closer_Text   => Comment_Closer & (Comment_Closer'Length + 1 .. Max_Bracket_Length => ' '),
         Opener_Length => Comment_Opener'Length,
         Closer_Length => Comment_Closer'Length,
         State         => Opener,
         Bracket_State => 1,
         Nested_Depth  => 0,
         Text          => Ada.Strings.Unbounded.Null_Unbounded_String);
   end Get;

   function Value (Item : in Instance) return String
   is begin
      return Ada.Strings.Unbounded.To_String (Item.Text);
   end Value;

end OpenToken.Recognizer.Bracketed_Comment;
