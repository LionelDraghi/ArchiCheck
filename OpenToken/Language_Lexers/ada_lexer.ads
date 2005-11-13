-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
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
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
-- $Log: ada_lexer.ads,v $
-- Revision 1.5  2000/08/07 00:17:43  Ted
-- Change to work w/ new package hierarchy
--
-- Revision 1.4  2000/01/27 21:21:05  Ted
-- Fix to work with 2.0
--
-- Revision 1.3  1999/12/27 19:56:05  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.2  1999/10/08 23:19:01  Ted
-- Disable sign recognition in integer and real literals
--
-- Revision 1.1  1999/08/17 03:40:24  Ted
-- Initial Version
--
--
-- 1.0 -  8 August 1999  Final complete version
-- 0.3 - 26 June 1999    Added character literals
-- 0.2 - 25 June 1999    Added based numbers
--                       (still missing: character literals and strings)
-- 0.1 - 23 June 1999    Bug fix (numeric literals)
-- 0.0 - 22 June 1999    First preliminary release
-------------------------------------------------------------------------------

with Opentoken.Token.Enumerated;
with Opentoken.Token.Enumerated.Analyzer;
with Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.Identifier;
with Opentoken.Recognizer.Graphic_Character, Opentoken.Recognizer.String;
with Opentoken.Recognizer.Integer, Opentoken.Recognizer.Based_Integer_Ada_Style,
     Opentoken.Recognizer.Real, Opentoken.Recognizer.Based_Real_Ada_Style;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.Line_Comment;
with Opentoken.Recognizer.End_Of_File;

pragma Elaborate_All (Opentoken.Token.Enumerated, Opentoken.Token.Enumerated.Analyzer,
                      Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator,
                      Opentoken.Recognizer.Identifier,
                      Opentoken.Recognizer.Graphic_Character, Opentoken.Recognizer.String,
                      Opentoken.Recognizer.Integer, Opentoken.Recognizer.Based_Integer_Ada_Style,
                      Opentoken.Recognizer.Real, Opentoken.Recognizer.Based_Real_Ada_Style,
                      Opentoken.Recognizer.Character_Set,
                      Opentoken.Recognizer.Line_Comment,
                      Opentoken.Recognizer.End_Of_File);

package Ada_Lexer is

  ---------------------------------------------------------------------
  -- This ia a lexical analyser for the Ada language.
  --
  -- There is another lexer for the Ada and Java languages at:
  --   <http://home.T-Online.de/home/Christ-Usch.Grein/Ada/Lexer.html>
  ---------------------------------------------------------------------

  type Ada_Token is
    (-- Reserved words ARM 2.9 (2)
     Abort_T, Abs_T, Abstract_T, Accept_T, Access_T, Aliased_T, All_T, And_T, Array_T, At_T,
     Begin_T, Body_T,
     Case_T, Constant_T,
     Declare_T, Delay_T, Delta_T, Digits_T, Do_T,
     Else_T, Elsif_T, End_T, Entry_T, Exception_T, Exit_T,
     For_T, Function_T,
     Generic_T, Goto_T,
     If_T, In_T, Is_T,
     Limited_T, Loop_T,
     Mod_T,
     New_T, Not_T, Null_T,
     Of_T, Or_T, Others_T, Out_T,
     Package_T, Pragma_T, Private_T, Procedure_T, Protected_T,
     Raise_T, Range_T, Record_T, Rem_T, Renames_T, Requeue_T, Return_T, Reverse_T,
     Select_T, Separate_T, Subtype_T,
     Tagged_T, Task_T, Terminate_T, Then_T, Type_T,
     Until_T, Use_T,
     When_T, While_T, With_T,
     Xor_T,
     -- Delimiters ARM 2.2 (9)
     -- & ' ( ) * + , - . / : ; < = > |
     -- Compound delimiters ARM 2.2 (14)
     -- => .. ** := /= >= <= << >> <>
     Colon_T, Comma_T, Dot_T, Semicolon_T, Tick_T,         -- : , . ; '
     Left_Parenthesis_T, Right_Parenthesis_T,              -- ( )
     Concatenate_T,                                        -- &
     Alternative_T,                                        -- |
     Equal_T, Not_Equal_T, Greater_Than_T, Less_Than_T,    -- = /= > <
     Greater_Equal_T, Less_Equal_T,                        -- >= <=
     Plus_T, Minus_T, Times_T, Divide_T,                   -- + - * /
     Arrow_T, Assignment_T, Double_Dot_T, Exponentiate_T,  -- => := .. **
     Left_Label_Bracket_T, Right_Label_Bracket_T, Box_T,   -- << >> <>
     -- Literals ARM 2.4 .. 2.6
     Integer_T,               -- 1, 1E+10
     Based_Integer_T,         -- 13#C#, 13#C#E+10
     Real_T,                  -- -3.141, 1.0E+10
     Based_Real_T,            -- 13#C.B#, 13#C.B#E+5
     Character_T, String_T,
     -- Other tokens
     Identifier_T,
     Comment_T,
     Whitespace_T,
     -- Syntax error
  -- Bad_Token_T,
     --
     End_of_File_T);

  package Master_Ada_Token is new Opentoken.Token.Enumerated (Ada_Token);
  package Tokenizer is new Master_Ada_Token.Analyzer;

  Syntax : constant Tokenizer.Syntax :=
    (Abort_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("abort")),
     Abs_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("abs")),
     Abstract_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("abstract")),
     Accept_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("accept")),
     Access_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("access")),
     Aliased_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("aliased")),
     All_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("all")),
     And_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("and")),
     Array_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("array")),
     At_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("at")),
     Begin_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("begin")),
     Body_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("body")),
     Case_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("case")),
     Constant_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("constant")),
     Declare_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("declare")),
     Delay_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("delay")),
     Delta_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("delta")),
     Digits_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("digits")),
     Do_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("do")),
     Else_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("else")),
     Elsif_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("elsif")),
     End_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("end")),
     Entry_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("entry")),
     Exception_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("exception")),
     Exit_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("exit")),
     For_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("for")),
     Function_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("function")),
     Generic_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("generic")),
     Goto_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("goto")),
     If_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("if")),
     In_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("in")),
     Is_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("is")),
     Limited_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("limited")),
     Loop_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("loop")),
     Mod_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("mod")),
     New_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("new")),
     Not_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("not")),
     Null_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("null")),
     Of_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("of")),
     Or_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("or")),
     Others_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("others")),
     Out_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("out")),
     Package_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("package")),
     Pragma_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("pragma")),
     Private_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("private")),
     Procedure_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("procedure")),
     Protected_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("protected")),
     Raise_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("raise")),
     Range_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("range")),
     Record_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("record")),
     Rem_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("rem")),
     Renames_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("renames")),
     Requeue_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("requeue")),
     Return_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("return")),
     Reverse_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("reverse")),
     Select_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("select")),
     Separate_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("separate")),
     Subtype_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("subtype")),
     Tagged_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("tagged")),
     Task_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("task")),
     Terminate_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("terminate")),
     Then_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("then")),
     Type_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("type")),
     Until_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("until")),
     Use_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("use")),
     When_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("when")),
     While_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("while")),
     With_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("with")),
     Xor_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("xor")),
     Colon_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (":")),
     Comma_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (",")),
     Dot_T                 => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (".")),
     Semicolon_T           => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (";")),
     Tick_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("'")),
     Left_Parenthesis_T    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("(")),
     Right_Parenthesis_T   => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (")")),
     Concatenate_T         => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("&")),
     Alternative_T         => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("|")),
     Equal_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("=")),
     Not_Equal_T           => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("/=")),
     Greater_Than_T        => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">")),
     Less_Than_T           => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<")),
     Greater_Equal_T       => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">=")),
     Less_Equal_T          => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<=")),
     Plus_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("+")),
     Minus_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("-")),
     Times_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("*")),
     Divide_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("/")),
     Arrow_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("=>")),
     Assignment_T          => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (":=")),
     Double_Dot_T          => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("..")),
     Exponentiate_T        => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("**")),
     Left_Label_Bracket_T  => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<<")),
     Right_Label_Bracket_T => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">>")),
     Box_T                 => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<>")),
     Integer_T       => Tokenizer.Get(Opentoken.Recognizer.Integer.Get (Allow_Signs => False)),
     Based_Integer_T => Tokenizer.Get(Opentoken.Recognizer.Based_Integer_Ada_Style.Get),
     Real_T          => Tokenizer.Get(Opentoken.Recognizer.Real.Get (Allow_Signs => False)),
     Based_Real_T    => Tokenizer.Get(Opentoken.Recognizer.Based_Real_Ada_Style.Get),
     Character_T     => Tokenizer.Get(Opentoken.Recognizer.Graphic_Character.Get),
     String_T        => Tokenizer.Get(Opentoken.Recognizer.String.Get),
     Identifier_T    => Tokenizer.Get(Opentoken.Recognizer.Identifier.Get),
     Comment_T       => Tokenizer.Get(Opentoken.Recognizer.Line_Comment.Get ("--")),
     Whitespace_T    => Tokenizer.Get(Opentoken.Recognizer.Character_Set.Get
                                      (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
     End_of_File_T   => Tokenizer.Get(Opentoken.Recognizer.End_Of_File.Get));

   Analyzer: Tokenizer.Instance := Tokenizer.Initialize (Syntax);

end Ada_Lexer;
