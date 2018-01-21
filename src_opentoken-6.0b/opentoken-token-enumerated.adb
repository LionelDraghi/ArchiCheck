-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2013, 2014 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
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

with Ada.Tags;
with Ada.Text_IO;
package body OpenToken.Token.Enumerated is

   overriding
   function Image (Token : in Instance) return String
   is begin
      return Token_Image (Token.ID) & (if Token.Has_Name then "." & Token.Name.all else "");
   end Image;

   procedure Free (Item : in out Handle)
   is begin
      Dispose (Item);
   end Free;

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class
   is begin
      if Name = "" then
         return Instance'Class (Instance'(Name => null, ID => ID, Build => Build));
      else
         return Instance'Class (Instance'(Name => new String'(Name), ID => ID, Build => Build));
      end if;
   end Get;

   function "+" (Item : in Token_ID) return Instance'Class
   is begin
      return Get (Item);
   end "+";

   procedure Set_Build (Token : in out Instance'Class; Build : in Action)
   is begin
      Token.Build := Build;
   end Set_Build;

   function Copy (Token : in Handle) return Handle
   is begin
      if Token = null then
         return null;
      else
         return new Class'(Token.all);
      end if;
   end Copy;

   function ID (Token : in Instance'Class) return Token_ID is
   begin
      return Token.ID;
   end ID;

   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID)
   is begin
      Token.ID := ID;
   end Set_ID;

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True)
   is
      use type Ada.Tags.Tag;
      Next_Token : constant OpenToken.Token.Class := Analyzer.Get;
   begin
      if Trace_Parse > 0 then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line (" enumerated " & Name_Dispatch (Match));
      end if;

      if Instance (Next_Token).ID = Match.ID then
         --  Next_Token was passed to a dispatching Create by
         --  Find_Next; copy the results of that.
         --
         --  Previous versions of OpenToken called Create again here,
         --  with an argument of Lexeme (Analyzer), which is wrong
         --  when Next_Token was read from the lookahead queue, since
         --  then Lexeme reads from the input buffer, which is not
         --  preserved in the lookahead queue. We added Copy to handle
         --  that; lexeme is preserved in the copy of the token in the
         --  lookahead queue.
         --
         --  Note that we can't just use ':='; that doesn't dispatch,
         --  so it only copies the ID, which is pointless.
         --
         --  This change (not calling Create here) will cause user
         --  applications to silently fail (silently meaning the
         --  compiler won't catch it; user unit tests should catch the
         --  problem). We can't make Copy abstract, because
         --  Enumerated.Instance can't be abstract.

         if Actively then
            if Next_Token'Tag = Class (Match.all)'Tag then
               Copy (To => Class (Match.all), From => Next_Token);
               if Match.Build /= null then
                  Match.Build (Match.all);
               end if;
            else
               --  It is the parser programmer's job to ensure these
               --  types match when the IDs do.
               raise Programmer_Error with
                 "Expected a token of type " & Ada.Tags.Expanded_Name (Class (Match.all)'Tag) &
                 "'Class but found a " & Ada.Tags.Expanded_Name (Next_Token'Tag);
            end if;
         end if;
      else
         if Actively then
            raise Parse_Error with "Expected " & Name_Dispatch (Match) & " but found " & Name_Dispatch (Next_Token);
         else
            --  The spec says "raise Parse_Error with no message". If
            --  we leave out 'with ""' here, GNAT attaches a message
            --  giving the line number. That's useful in general, but
            --  fails our unit test. And it would be painful (and
            --  non-portable) to maintain a unit test that checked for
            --  the GNAT string, since it has line numbers in it. So
            --  we override it in this case.
            --
            --  The point of "raise with no message" is to not spend
            --  time computing a nice user message, because it will be
            --  thrown away. I'm not clear whether 'with ""' or the
            --  GNAT default is faster, but it probably doesn't matter
            --  much.
            raise Parse_Error with "";
         end if;
      end if;

      Analyzer.Find_Next (Look_Ahead => not Actively);

      if Trace_Parse > 0 then
         Trace_Put ("...succeeded"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
   exception
   when others =>
      if Trace_Parse > 0 then
         Trace_Put ("...failed"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
      raise;
   end Parse;

   overriding function Name (Token : in Instance) return String
   is begin
      if Token.Name = null then
         return Token_Image (Token.ID);
      else
         return Token.Name.all;
      end if;
   end Name;

end OpenToken.Token.Enumerated;
