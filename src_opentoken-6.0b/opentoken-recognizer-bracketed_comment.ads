-------------------------------------------------------------------------------
--
-- Copyright (C) 2012 Stephen Leake
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
--  This package implements a token recognizer for a comment that is
--  bracketed by special comment opening and closing character
--  strings. The text in between may extend over several lines.
--
--  A line ending in the returned Lexeme is represented by
--  OpenToken.EOL_Character, which may be different from what your
--  current operating system uses.
------------------------------------------------------------------------------
with Ada.Strings.Unbounded;
package OpenToken.Recognizer.Bracketed_Comment is

   Max_Bracket_Length : constant := 10;

   type Instance is new OpenToken.Recognizer.Instance with private;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a Comment token. The
   --  opener and closer can be no longer than Max_Bracket_Length.
   --------------------------------------------------------------------------
   function Get
     (Comment_Opener : in String;
      Comment_Closer : in String;
      Reportable     : in Boolean := False;
      Nested         : in Boolean := False)
     return Instance;

   function Value (Item : in Instance) return String;

private

   type State_ID is (Opener, Nest_Opener, Text, Nest_Closer, Closer, Done);

   --  An alternate design, to avoid the hard-coded
   --  Max_Bracket_Length, would be to declare a discriminated
   --  Delimiters_Type holding Opener_Text and Closer_Text. Then
   --  Instance would contain a Delimeter_Type'Access, and Get would
   --  allocate an object of that type.

   subtype Bracketed_String is String (1 .. Max_Bracket_Length);
   type Instance is new OpenToken.Recognizer.Instance with record

      Nested         : Boolean := False;

      --  The comment introducer string
      Opener_Text   : Bracketed_String;
      Closer_Text   : Bracketed_String;
      Opener_Length : Natural := 0;
      Closer_Length : Natural := 0;

      --  The finite state machine state
      State          : State_ID := Opener;
      Bracket_State  : Positive := 1;
      Nested_Depth   : Natural := 0;

      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Bracketed_Comment;
