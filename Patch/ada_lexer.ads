-------------------------------------------------------------------------------
--
-- Copyright (C) 2012 Stephen Leake
-- Copyright (C) 1999, 2008 Christoph Karl Walter Grein
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
-------------------------------------------------------------------------------

with Ada.Text_IO;

package Ada_Lexer is

   ---------------------------------------------------------------------
   --  This is a lexical analyser for the Ada 2012 language.
   --
   --  There is another lexer for the Ada and Java languages at:
   --   <http://home.T-Online.de/home/Christ-Usch.Grein/Ada/Lexer.html>
   ---------------------------------------------------------------------

   type Ada_Token is
     (
      --  Reserved words ARM 2.9 (2)
      Abort_T, Abs_T, Abstract_T, Accept_T, Access_T, Aliased_T, All_T, And_T, Array_T, At_T,
      Begin_T, Body_T,
      Case_T, Constant_T,
      Declare_T, Delay_T, Delta_T, Digits_T, Do_T,
      Else_T, Elsif_T, End_T, Entry_T, Exception_T, Exit_T,
      For_T, Function_T,
      Generic_T, Goto_T,
      If_T, In_T, Interface_T, Is_T,
      Limited_T, Loop_T,
      Mod_T,
      New_T, Not_T, Null_T,
      Of_T, Or_T, Others_T, Out_T, Overriding_T,
      Package_T, Pragma_T, Private_T, Procedure_T, Protected_T,
      Raise_T, Range_T, Record_T, Rem_T, Renames_T, Requeue_T, Return_T, Reverse_T,
      Select_T, Separate_T, Some_T, Subtype_T, Synchronized_T,
      Tagged_T, Task_T, Terminate_T, Then_T, Type_T,
      Until_T, Use_T,
      When_T, While_T, With_T,
      Xor_T,
      --  Delimiters ARM 2.2 (9)
      --  & ' ( ) * + , - . / : ; < = > |
      --  Compound delimiters ARM 2.2 (11)
      --  => .. ** := /= >= <= << >> <>
      Colon_T, Comma_T, Dot_T, Semicolon_T, Tick_T,         -- : , . ; '
      Left_Parenthesis_T, Right_Parenthesis_T,              -- ( )
      Concatenate_T,                                        -- &
      Alternative_T,                                        -- |
      Equal_T, Not_Equal_T, Greater_Than_T, Less_Than_T,    -- = /= > <
      Greater_Equal_T, Less_Equal_T,                        -- >= <=
      Plus_T, Minus_T, Times_T, Divide_T,                   -- + - * /
      Arrow_T, Assignment_T, Double_Dot_T, Exponentiate_T,  -- => := .. **
      Left_Label_Bracket_T, Right_Label_Bracket_T, Box_T,   -- << >> <>

      --  Literals ARM 2.4 .. 2.6
      Integer_T,               -- 1, 1E+10
      Based_Integer_T,         -- 13#C#, 13#C#E+10
      Real_T,                  -- -3.141, 1.0E+10
      Based_Real_T,            -- 13#C.B#, 13#C.B#E+5
      Character_T, String_T,
      --  Other tokens
      Identifier_T,
      Comment_T,
      Whitespace_T,
      --  Syntax error
      Bad_Token_T,
      --
      End_of_File_T);

   --  Define the file where to find the code to be processed.
   --  The file must be open for reading.
   procedure Set_Input_Feeder (File : in Ada.Text_IO.File_Type);

   --  In case of syntax errors:
   --  Define whether the Syntax_Error exception shall be raised (default)
   --  or the Bad_Token_T token shall be be returned.
   procedure Exception_on_Syntax_Error;
   procedure Bad_Token_on_Syntax_Error;

   --  Change reportability of comments (off by default).
   procedure Set_Comments_Reportable (To : in Boolean);

   --  Find the next reportable token.
   procedure Find_Next;

   --  Query the current token:

   function Line   return Natural;
   function Column return Natural;

   function Token_ID return Ada_Token;
   function Lexeme   return String;

end Ada_Lexer;
