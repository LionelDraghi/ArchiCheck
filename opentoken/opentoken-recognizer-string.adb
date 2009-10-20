-------------------------------------------------------------------------------
--
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

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a string literal.
--  It can optionally use an escape character to introduce special
--  character mappings, and can thus be used to recognize either Ada
--  or C-style strings.
-----------------------------------------------------------------------------
package body OpenToken.Recognizer.String is

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   : out    Analysis_Verdict)
   is
   begin
      case The_Token.State is
         --  A string must start with the string delimiter
         when Delimit =>
            if Next_Char = The_Token.Delimiter then

               The_Token.State := Text;
               Verdict := So_Far_So_Good;

            elsif Next_Char = EOL_Character then

               The_Token.State := Done;
               Verdict := Failed;

            else
               The_Token.State := Done;
               Verdict := Failed;
            end if;

         when Text =>
            --  Process normal text contents
            if Next_Char = The_Token.Delimiter then

               --  If its a delimiter, report that we have a good
               --  string, but look for the next character to possibly
               --  be another delimiter.

               if The_Token.Double_Delimiter then
                  The_Token.State := Double_Delimit;
               else
                  The_Token.State := Done;
               end if;

               The_Token.Good_Length := The_Token.Value_Length;
               Verdict := Matches;

            elsif Next_Char = EOL_Character then

               The_Token.State := Done;
               Verdict := Failed;

               --  If its an escape character and we are using escape
               --  characters go into escape mode.
            elsif Next_Char = The_Token.Escape then
               if The_Token.Escapeable then
                  The_Token.State := Escaped_Text;
                  Verdict := So_Far_So_Good;
               else
                  The_Token.State := Text;
                  Verdict := So_Far_So_Good;
                  The_Token.Value_Length := The_Token.Value_Length + 1;
                  The_Token.Value (The_Token.Value_Length) := Next_Char;
               end if;

            else
               The_Token.State := Text;
               Verdict := So_Far_So_Good;
               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) := Next_Char;
            end if;

         --  Process escaped characters
         when Escaped_Text =>

            --  If its a number, start to calculate its value.
            if Next_Char in '0' .. '7' then
               The_Token.State := Escaped_Octal_Number;
               The_Token.Esc_Code := Natural'Value ((1 => Next_Char));
               Verdict := So_Far_So_Good;

            --  ..otherwise, if its an x, the next characters should be hex digits
            elsif Next_Char = 'x' then
               The_Token.State := First_Hex_Digit;
               Verdict := So_Far_So_Good;

            --  ...otherwise, return the mapped value for the character.
            else
               The_Token.State := Text;
               Verdict := So_Far_So_Good;

               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) :=
                 Ada.Strings.Maps.Value (Map     => The_Token.Escape_Mapping,
                                         Element => Next_Char);

            end if;

         --  Process escaped octal numbers
         when Escaped_Octal_Number =>
            --  If its a number, continue to calculate its value.
            if Next_Char in '0' .. '7' then
               The_Token.Esc_Code := The_Token.Esc_Code * 8 + Natural'Value ((1 => Next_Char));

               --  Verify that the number isn't too large to be a character value
               if The_Token.Esc_Code <= Character'Pos (Character'Last) then
                  The_Token.State := Escaped_Octal_Number;
                  Verdict := So_Far_So_Good;
               else
                  The_Token.State := Done;
                  Verdict := Failed;
               end if;


            elsif Next_Char = EOL_Character then

               The_Token.State := Done;
               Verdict := Failed;

            --  ...otherwise, return the character value for the given number, and process the character as text
            else
               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) := Character'Val (The_Token.Esc_Code);

               if Next_Char = The_Token.Delimiter then
                  --  If its a delimiter, report that we have a good
                  --  string, but look for the next character to
                  --  possibly be another delimiter.

                  if The_Token.Double_Delimiter then
                     The_Token.State := Double_Delimit;
                  else
                     The_Token.State := Done;
                  end if;

                  The_Token.Good_Length := The_Token.Value_Length;
                  Verdict := Matches;
               elsif Next_Char = The_Token.Escape then
                  The_Token.State := Escaped_Text;
                  Verdict := So_Far_So_Good;
               else
                  The_Token.State := Text;
                  Verdict := So_Far_So_Good;
                  The_Token.Value_Length := The_Token.Value_Length + 1;
                  The_Token.Value (The_Token.Value_Length) := Next_Char;
               end if;

            end if;

         --  Process the first hex digit
         when First_Hex_Digit =>
            --  If its a number, continue to calculate its value.
            if Next_Char in '0' .. '9' then
               The_Token.Esc_Code := Natural'Value ((1 => Next_Char));

               The_Token.State := Escaped_Hex_Number;
               Verdict := So_Far_So_Good;

            elsif Next_Char in 'a' .. 'f' then
               The_Token.Esc_Code := Character'Pos (Next_Char) - (Character'Pos ('a') - 10);

               The_Token.State := Escaped_Hex_Number;
               Verdict := So_Far_So_Good;

            elsif Next_Char in 'A' .. 'F' then
               The_Token.Esc_Code := Character'Pos (Next_Char) - (Character'Pos ('A') - 10);

               The_Token.State := Escaped_Hex_Number;
               Verdict := So_Far_So_Good;

            elsif Next_Char = EOL_Character then

               The_Token.State := Done;
               Verdict := Failed;

            --  ...otherwise, return the character value for the given
            --  number, and process the character as text
            else
               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) := Character'Val (The_Token.Esc_Code);

               if Next_Char = The_Token.Delimiter then
                  --  If its a delimiter, report that we have a good
                  --  string, but look for the next character to
                  --  possibly be another delimiter.

                  if The_Token.Double_Delimiter then
                     The_Token.State := Double_Delimit;
                  else
                     The_Token.State := Done;
                  end if;

                  The_Token.Good_Length := The_Token.Value_Length;
                  Verdict := Matches;
               elsif Next_Char = The_Token.Escape then
                  The_Token.State := Escaped_Text;
                  Verdict := So_Far_So_Good;
               else
                  The_Token.State := Text;
                  Verdict := So_Far_So_Good;
                  The_Token.Value_Length := The_Token.Value_Length + 1;
                  The_Token.Value (The_Token.Value_Length) := Next_Char;
               end if;

            end if;

         --  Process the first hex digit
         when Escaped_Hex_Number =>
            --  If its a number, continue to calculate its value.
            if Next_Char in '0' .. '9' then
               The_Token.Esc_Code := The_Token.Esc_Code * 16 + Natural'Value ((1 => Next_Char));

               The_Token.State := Text;
               Verdict := So_Far_So_Good;

            elsif Next_Char in 'a' .. 'f' then
               The_Token.Esc_Code := The_Token.Esc_Code * 16 +
                 Character'Pos (Next_Char) - (Character'Pos ('a') - 10);

               The_Token.State := Text;
               Verdict := So_Far_So_Good;

            elsif Next_Char in 'A' .. 'F' then
               The_Token.Esc_Code := The_Token.Esc_Code * 16 +
                 Character'Pos (Next_Char) - (Character'Pos ('A') - 10);

               The_Token.State := Text;
               Verdict := So_Far_So_Good;

            elsif Next_Char = EOL_Character then

               The_Token.State := Done;
               Verdict := Failed;

            --  ...otherwise, return the character value for the given
            --  number, and process the character as text
            else
               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) := Character'Val (The_Token.Esc_Code);

               if Next_Char = The_Token.Delimiter then
                  --  If its a delimiter, report that we have a good
                  --  string, but look for the next character to
                  --  possibly be another delimiter.

                  if The_Token.Double_Delimiter then
                     The_Token.State := Double_Delimit;
                  else
                     The_Token.State := Done;
                  end if;

                  The_Token.Good_Length := The_Token.Value_Length;
                  Verdict := Matches;
               elsif Next_Char = The_Token.Escape then
                  The_Token.State := Escaped_Text;
                  Verdict := So_Far_So_Good;
               else
                  The_Token.State := Text;
                  Verdict := So_Far_So_Good;
                  The_Token.Value_Length := The_Token.Value_Length + 1;
                  The_Token.Value (The_Token.Value_Length) := Next_Char;
               end if;

            end if;

         --  Process the character after the delimiter
         when Double_Delimit =>

            --  if its another delimiter, insert the delimiter
            --  character itself and keep going.
            if Next_Char = The_Token.Delimiter then
               The_Token.State := Text;
               Verdict := So_Far_So_Good;

               The_Token.Value_Length := The_Token.Value_Length + 1;
               The_Token.Value (The_Token.Value_Length) := The_Token.Delimiter;
            else
               The_Token.State := Done;
               Verdict := Failed;
            end if;

         --  It can't possibly match after this...
         when Done =>
            Verdict := Failed;
      end case;
   end Analyze;

   overriding procedure Clear (The_Token : in out Instance) is
   begin
      The_Token.State        := Delimit;
      The_Token.Value_Length := 0;
      The_Token.Good_Length  := 0;
   end Clear;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a String literal token.
   --  If Escapable is set to False, the string will treat the Escape
   --  character as any other character.
   --
   --  If all parameters are defaulted, an Ada-style string token will
   --  be created. If Escapeable is set to True and all other
   --  parameters are defaulted, a C-style string token will be
   --  created.
   --------------------------------------------------------------------------
   function Get
     (Delimiter        : in Character                          := '"';
      Double_Delimiter : in Boolean                            := True;
      Escapeable       : in Boolean                            := False;
      Escape           : in Character                          := '\';
      Escape_Mapping   : in Ada.Strings.Maps.Character_Mapping := C_Style_Escape_Code_Map)
     return Instance
   is begin
      return (Report           => True,
              State            => Delimit,
              Esc_Code         => 0,
              Delimiter        => Delimiter,
              Double_Delimiter => Double_Delimiter,
              Escapeable       => Escapeable,
              Escape           => Escape,
              Escape_Mapping   => Escape_Mapping,
              Value            => (others => Ada.Characters.Latin_1.NUL),
              Value_Length     => 0,
              Good_Length      => 0
              );
   end Get;

   --------------------------------------------------------------------------
   --  Return the translated value of the recognized string. This will
   --  not include any quotation characters. The escape mapping will
   --  have been applied, and internal sets of double quotes will
   --  appear as a single double-quote character.
   --------------------------------------------------------------------------
   function Value (Recognized_String : in Instance) return Standard.String is
   begin
      return Recognized_String.Value (1 .. Recognized_String.Good_Length);
   end Value;

end OpenToken.Recognizer.String;

