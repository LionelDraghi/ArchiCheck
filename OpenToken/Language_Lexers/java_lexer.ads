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
-- $Log: java_lexer.ads,v $
-- Revision 1.5  2000/08/06 23:37:55  Ted
-- Change to work w/ new package hierarchy
--
-- Revision 1.4  2000/01/27 21:21:05  Ted
-- Fix to work with 2.0
--
-- Revision 1.3  1999/12/27 19:56:06  Ted
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
-- 0.2 -  4 July   1999  Exclusion set for characters
-- 0.1 - 28 June   1999  Added escape sequences and /* bracketed comment */
-- 0.0 - 27 June   1999  First preliminary release
-------------------------------------------------------------------------------

with Ada.Strings.Maps;

with Opentoken.Token.Enumerated.Analyzer;
with Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.Identifier;
with Opentoken.Recognizer.Graphic_Character,
     Opentoken.Recognizer.Escape_Sequence, Opentoken.Recognizer.Octal_Escape, Opentoken.Recognizer.String;
with Opentoken.Recognizer.Integer, Opentoken.Recognizer.Based_Integer_Java_Style,
     Opentoken.Recognizer.Real;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.Line_Comment, Opentoken.Recognizer.Bracketed_Comment;
with Opentoken.Recognizer.End_Of_File;

pragma Elaborate_All (Opentoken.Token.Enumerated.Analyzer,
                      Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator,
                      Opentoken.Recognizer.Identifier,
                      Opentoken.Recognizer.Graphic_Character,
                      Opentoken.Recognizer.Escape_Sequence, Opentoken.Recognizer.Octal_Escape, Opentoken.Recognizer.String,
                      Opentoken.Recognizer.Integer, Opentoken.Recognizer.Based_Integer_Java_Style,
                      Opentoken.Recognizer.Real,
                      Opentoken.Recognizer.Character_Set,
                      Opentoken.Recognizer.Line_Comment, Opentoken.Recognizer.Bracketed_Comment,
                      Opentoken.Recognizer.End_Of_File);

package Java_Lexer is

  ---------------------------------------------------------------------
  -- This ia a lexical analyser for the Java language.
  -- In the current preliminary state, not all tokens are recognized.
  --
  -- Missing:
  --   Numerals with suffixes
  --     integer suffixes l L
  --     float suffixes d D f F
  --
  -- There is another lexer for the Ada and Java languages at:
  --   <http://home.T-Online.de/home/Christ-Usch.Grein/Ada/Lexer.html>
  ---------------------------------------------------------------------

  type Java_Token is
    (-- Keywords JRM 3.9
     Abstract_T,
     Boolean_T, Break_T, Byte_T,
     Case_T, Catch_T, Char_T, Class_T, Const_T, Continue_T,
     Default_T, Do_T, Double_T,
     Else_T, Extends_T,
     Final_T, Finally_T, Float_T, For_T,
     Goto_T,
     If_T, Implements_T, Import_T, InstanceOf_T, Int_T, Interface_T,
     Long_T,
     Native_T, New_T,
     Package_T, Private_T, Protected_T, Public_T,
     Return_T,
     Short_T, Static_T, Super_T, Switch_T, Synchronized_T,
     This_T, Throw_T, Throws_T, Transient_T, Try_T,
     Void_T, Volatile_T,
     While_T,
     -- Separators JRM 3.11
     -- ( ) { } [ ] ; , .
     -- Operators JRM 3.12
     -- =  >  <  !  ~  ?  :
     -- == <= >= != && || ++ --
     -- +  -  *  /  &  |  ^  %  <<  >>  >>>
     -- += -= *= /= &= |= ^= %= <<= >>= >>>=
     Colon_T, Comma_T, Dot_T, Semicolon_T,             -- : , . ;
     LeftBrace_T, RightBrace_T,                        -- { }
     LeftBracket_T, RightBracket_T,                    -- [ ]
     Left_Parenthesis_T, Right_Parenthesis_T,          -- ( )
     And_T, Or_T,                                      -- & |
     ShortCutAnd_T, ShortCutOr_T,                      -- && ||
     Assignment_T, Conditional_T,                      -- = ?
     Equal_T, NotEqual_T,                              -- == !=
     Greater_Equal_T, Less_Equal_T,                    -- >= <=
     Greater_Than_T, Less_Than_T,                      -- > <
     Complement_T, Not_T, Xor_T,                       -- ~ ! ^
     Plus_T, Minus_T, Times_T, Divide_T, Remainder_T,  -- + - * / %
     Increment_T, Decrement_T,                         -- ++ --
     LeftShift_T, RightShift_T, UnsignedRightShift_T,  -- << >> >>>
     PlusAssign_T, MinusAssign_T,                      -- += -=
     TimesAssign_T, DivideAssign_T, RemainderAssign_T, -- *= /= %=
     AndAssign_T, OrAssign_T, XorAssign_T,             -- &= |= ^=
     LeftShiftAssign_T, RightShiftAssign_T,            -- <<= >>=
     UnsignedRightShiftAssign_T,                       -- >>>=
     -- Literals (JRM 3.10) (all Java reals may use lazy forms,
     -- i.e. the whole or decimal part may be missing)
     Null_T, False_T, True_T,
     Integer_T,           -- 1
     Based_Integer_T,     -- 07, 0xF
   --LongInteger_T,       -- 1L
   --BasedLongInteger_T,  -- 07L, 0xFL
     Real_T,              -- 1.0, 1., .1, 1E+7
   --FloatNumber_T,       -- 1.0E+10F
   --DoubleNumber_T,      -- 1.0E+10D
     Character_T,         -- 'x' with x any graphic character except one of "'\
     Escape_Sequence_T,   -- '\x' with x one of btnfr"'\
     Octal_Escape_T,      -- '\377'
     String_T,            -- "Any characters except " or \ and escape sequences"
     -- Other tokens
     Identifier_T,
     EndOfLineComment_T,  -- // to end of line
     EmbeddedComment_T,   -- /* anything (even several lines) */
     Whitespace_T,
     -- Syntax error
  -- Bad_Token_T,
     --
     End_of_File_T);

  package Master_Java_Token is new Opentoken.Token.Enumerated (Java_Token);
  package Tokenizer is new Master_Java_Token.Analyzer;

  Syntax : constant Tokenizer.Syntax :=
    (Abstract_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("abstract"    , Case_Sensitive => True)),
     Boolean_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("boolean"     , Case_Sensitive => True)),
     Break_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("break"       , Case_Sensitive => True)),
     Byte_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("byte"        , Case_Sensitive => True)),
     Case_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("case"        , Case_Sensitive => True)),
     Catch_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("catch"       , Case_Sensitive => True)),
     Char_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("char"        , Case_Sensitive => True)),
     Class_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("class"       , Case_Sensitive => True)),
     Const_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("const"       , Case_Sensitive => True)),
     Continue_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("continue"    , Case_Sensitive => True)),
     Default_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("default"     , Case_Sensitive => True)),
     Do_T           => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("do"          , Case_Sensitive => True)),
     Double_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("double"      , Case_Sensitive => True)),
     Else_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("else"        , Case_Sensitive => True)),
     Extends_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("extends"     , Case_Sensitive => True)),
     Final_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("final"       , Case_Sensitive => True)),
     Finally_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("finally"     , Case_Sensitive => True)),
     Float_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("float"       , Case_Sensitive => True)),
     For_T          => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("for"         , Case_Sensitive => True)),
     Goto_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("goto"        , Case_Sensitive => True)),
     If_T           => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("if"          , Case_Sensitive => True)),
     Implements_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("implements"  , Case_Sensitive => True)),
     Import_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("import"      , Case_Sensitive => True)),
     InstanceOf_T   => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("instanceof"  , Case_Sensitive => True)),
     Int_T          => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("int"         , Case_Sensitive => True)),
     Interface_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("interface"   , Case_Sensitive => True)),
     Long_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("long"        , Case_Sensitive => True)),
     Native_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("native"      , Case_Sensitive => True)),
     New_T          => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("new"         , Case_Sensitive => True)),
     Package_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("package"     , Case_Sensitive => True)),
     Private_T      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("private"     , Case_Sensitive => True)),
     Protected_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("protected"   , Case_Sensitive => True)),
     Public_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("public"      , Case_Sensitive => True)),
     Return_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("return"      , Case_Sensitive => True)),
     Short_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("short"       , Case_Sensitive => True)),
     Static_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("static"      , Case_Sensitive => True)),
     Super_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("super"       , Case_Sensitive => True)),
     Switch_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("switch"      , Case_Sensitive => True)),
     Synchronized_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("synchronized", Case_Sensitive => True)),
     This_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("this"        , Case_Sensitive => True)),
     Throw_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("throw"       , Case_Sensitive => True)),
     Throws_T       => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("throws"      , Case_Sensitive => True)),
     Transient_T    => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("transient"   , Case_Sensitive => True)),
     Try_T          => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("try"         , Case_Sensitive => True)),
     Void_T         => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("void"        , Case_Sensitive => True)),
     Volatile_T     => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("volatile"    , Case_Sensitive => True)),
     While_T        => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("while"       , Case_Sensitive => True)),
     Colon_T                    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (":")),
     Comma_T                    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (",")),
     Dot_T                      => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (".")),
     Semicolon_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (";")),
     LeftBrace_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("{")),
     RightBrace_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("}")),
     LeftBracket_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("[")),
     RightBracket_T             => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("]")),
     Left_Parenthesis_T         => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("(")),
     Right_Parenthesis_T        => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (")")),
     And_T                      => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("&")),
     Or_T                       => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("|")),
     ShortCutAnd_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("&&")),
     ShortCutOr_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("||")),
     Assignment_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("=")),
     Conditional_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("?")),
     Equal_T                    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("==")),
     NotEqual_T                 => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("!=")),
     Greater_Equal_T            => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">=")),
     Less_Equal_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<=")),
     Greater_Than_T             => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">")),
     Less_Than_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<")),
     Complement_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("~")),
     Not_T                      => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("!")),
     Xor_T                      => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("^")),
     Plus_T                     => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("+")),
     Minus_T                    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("-")),
     Times_T                    => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("*")),
     Divide_T                   => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("/")),
     Remainder_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("%")),
     Increment_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("++")),
     Decrement_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("--")),
     LeftShift_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<<")),
     RightShift_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">>")),
     UnsignedRightShift_T       => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">>>")),
     PlusAssign_T               => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("+=")),
     MinusAssign_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("-=")),
     TimesAssign_T              => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("*=")),
     DivideAssign_T             => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("/=")),
     RemainderAssign_T          => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("%=")),
     AndAssign_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("&=")),
     OrAssign_T                 => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("|=")),
     XorAssign_T                => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("^=")),
     LeftShiftAssign_T          => Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("<<=")),
     RightShiftAssign_T         => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">>=")),
     UnsignedRightShiftAssign_T => Tokenizer.Get(Opentoken.Recognizer.Separator.Get (">>>=")),
     Null_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("null" , Case_Sensitive => True)),
     False_T => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("false", Case_Sensitive => True)),
     True_T  => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("true" , Case_Sensitive => True)),
     Integer_T       => Tokenizer.Get(Opentoken.Recognizer.Integer.Get (Allow_Underscores  => False,
                                                                       Allow_Exponent     => False,
                                                                       Allow_Signs        => False,
                                                                       Allow_Leading_Zero => False)),
     Based_Integer_T => Tokenizer.Get(Opentoken.Recognizer.Based_Integer_Java_Style.Get),
     Real_T          => Tokenizer.Get(Opentoken.Recognizer.Real.Get (Allow_Underscores => False,
                                                                 Allow_Signs       => False,
                                                                 Allow_Laziness    => True)),
     Identifier_T  => Tokenizer.Get(Opentoken.Recognizer.Identifier.Get),
     Character_T       => Tokenizer.Get(Opentoken.Recognizer.Graphic_Character.Get
                                      (Exclude => Ada.Strings.Maps.To_Set ("""'\"))),
     Escape_Sequence_T => Tokenizer.Get(Opentoken.Recognizer.Escape_Sequence.Get
                                      (Ada.Strings.Maps.To_Set ("btnfr""'\"))),
     Octal_Escape_T    => Tokenizer.Get(Opentoken.Recognizer.Octal_Escape.Get),
     String_T          => Tokenizer.Get(Opentoken.Recognizer.String.Get
                                      (Escapeable       => True,
                                       Double_Delimiter => False,
                                       Escape_Mapping   => Opentoken.Recognizer.String.Java_Style_Escape_Code_Map)),
     EndOfLineComment_T => Tokenizer.Get(Opentoken.Recognizer.Line_Comment.Get ("//")),
     EmbeddedComment_T  => Tokenizer.Get(Opentoken.Recognizer.Bracketed_Comment.Get ("/*", "*/")),
     Whitespace_T  => Tokenizer.Get(Opentoken.Recognizer.Character_Set.Get
                                      (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
     End_of_File_T => Tokenizer.Get(Opentoken.Recognizer.End_Of_File.Get));

   Analyzer: Tokenizer.Instance := Tokenizer.Initialize (Syntax);

end Java_Lexer;
