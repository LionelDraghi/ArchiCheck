-------------------------------------------------------------------------------
--
--  Copyright (C) 2012, 2013, 2014 Stephen Leake
--  Copyright (C) 2000 David Starner
--  Copyright (C) 1999 Christoph Karl Walter Grein
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it and/or
--  modify it under the terms of the  GNU General Public License as published
--  by the Free Software Foundation; either version 3, or (at your option)
--  any later version. The OpenToken package is distributed in the hope that
--  it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for  more details.  You should have received
--  a copy of the GNU General Public License  distributed with the OpenToken
--  package;  see file GPL.txt.  If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License.  This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This is a lexer for the Modula 3 language, as defined by the Modula 3
--  standard at http://www.research.digital.com/SRC/m3defn/html/complete.html.
--  It's a work in process, and is designed as the lexer for a m3toAda
--  translator.
-------------------------------------------------------------------------------
with Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Graphic_Character;
with OpenToken.Recognizer.Escape_Sequence;
with OpenToken.Recognizer.Octal_Escape;
with OpenToken.Recognizer.String;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Real;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.End_Of_File;
package M3_Lexer is

   type M3_Token is
     (
      --  Keywords and reserved identifiers
      ABS_T, ADDRESS_T, ADR_T, ADRSIZE_T, AND_T, ANY_T, ARRAY_T, AS_T,
      BEGIN_T, BITS_T, BITSIZE_T, BOOLEAN_T, BRANDED_T, BY_T, BYTESIZE_T,
      CARDINAL_T, CASE_T, CEILING_T, CHAR_T, CONST_T,
      DEC_T, DISPOSE_T, DIV_T, DO_T,
      ELSE_T, ELSIF_T, END_T, EVAL_T, EXCEPT_T, EXCEPTION_T, EXIT_T,
      EXPORTS_T, EXTENDED_T,
      FALSE_T, FINALLY_T, FIRST_T, FLOAT_T, FLOOR_T, FOR_T, FROM_T,
      GENERIC_T,
      IF_T, IMPORT_T, IN_T, INC_T, INTEGER_T, INTERFACE_T, ISTYPE_T,
      LAST_T, LOCK_T, LONGREAL_T, LOOP_T, LOOPHOLE_T,
      MAX_T, METHODS_T, MIN_T, MOD_T, MODULE_T, MUTEX_T,
      NARROW_T, NEW_T, NIL_T, NOT_T, NULL_T, NUMBER_T,
      OBJECT_T, OF_T, OR_T, ORD_T, OVERRIDES_T,
      PROCEDURE_T,
      RAISE_T, RAISES_T, READONLY_T, REAL_T, RECORD_T, REF_T, REFANY_T,
      REPEAT_T, RETURN_T, REVEAL_T, ROOT_T, ROUND_T,
      SET_T, SUBARRAY_T,
      TEXT_T, THEN_T, TRUE_T, TRUNC_T, TRY_T, TYPE_T, TYPECASE_T, TYPECODE_T,
      UNSAFE_T, UNTIL_T, UNTRACED_T,
      VALUE_T, VAL_T, VAR_T,
      WHILE_T, WITH_T,
      --  Operators
      Plus_T, Minus_T, Splat_T, Slant_T, GT_T, LT_T, GTE_T, LTE_T,
      Octothorpe_T, Embrace_T, Bracelet_T, Wax_T, Wane_T, Bracket_T,
      Unbracket_T, Equals_T, Semicolon_T, Spike_T, Caret_T, Dot_T,
      Range_T, Assign_T, Comma_T, Ampersand_T, Colon_T, Subtype_T,
      Arrow_T,
      --  Literals
      Integer_Number_T,           -- 1

      --  Based_Integer_T,     -- 12_AAA0

      Real_Number_T,              -- 1.0, 1.1E1, 1.1E+7; not .1, 1E+7
      Character_T,         -- 'x' with x a graphic character except one of "'\
      Escape_Sequence_T,   -- '\x' with x one of tnfr"'\
      Octal_Escape_T,      -- '\377', '\001'
      String_T,            -- "Anything except " or \ and escape sequences"

      --  Other tokens
      Identifier_T,
      Comment_T,   -- (* (* nested *) anything (even several lines) *)
      Whitespace_T,
      End_of_File_T);

   package Master_Token is new OpenToken.Token.Enumerated (M3_Token, M3_Token'First, M3_Token'Last, M3_Token'Image);
   package Tokenizer is new Master_Token.Analyzer;

   M3_Style_Escape_Code_Map : constant Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.To_Mapping
     (From => "tnfr""'\",
      To => Ada.Characters.Latin_1.HT &
        Ada.Characters.Latin_1.LF &
        Ada.Characters.Latin_1.FF &
        Ada.Characters.Latin_1.CR & '"' & ''' & '\');

   M3_Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
     (Ada.Characters.Latin_1.Space &
        Ada.Characters.Latin_1.HT &
        Ada.Characters.Latin_1.VT &
        Ada.Characters.Latin_1.FF &
        OpenToken.EOL_Character);

   Syntax : constant Tokenizer.Syntax :=
     (ABS_T                       => Tokenizer.Get
        (OpenToken.Recognizer.Keyword.Get
           ("ABS", Case_Sensitive => True)),
      ADDRESS_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ADDRESS", True)),
      ADR_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ADR", True)),
      ADRSIZE_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ADRSIZE", True)),
      AND_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("AND", True)),
      ANY_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ANY", True)),
      ARRAY_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ARRAY", True)),
      AS_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("AS", True)),
      BEGIN_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BEGIN", True)),
      BITS_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BITS", True)),
      BITSIZE_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BITSIZE", True)),
      BOOLEAN_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BOOLEAN", True)),
      BRANDED_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BRANDED", True)),
      BY_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BY", True)),
      BYTESIZE_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("BYTESIZE", True)),
      CARDINAL_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CARDINAL", True)),
      CASE_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CASE", True)),
      CEILING_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CEILING", True)),
      CHAR_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CHAR", True)),
      CONST_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CONST", True)),
      DEC_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("DEC", True)),
      DISPOSE_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("DISPOSE", True)),
      DIV_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("DIV", True)),
      DO_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("DO", True)),
      ELSE_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ELSE", True)),
      ELSIF_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ELSIF", True)),
      END_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("END", True)),
      EVAL_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EVAL", True)),
      EXCEPT_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EXCEPT", True)),
      EXCEPTION_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EXCEPTION", True)),
      EXIT_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EXIT", True)),
      EXPORTS_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EXPORTS", True)),
      EXTENDED_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("EXTENDED", True)),
      FALSE_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FALSE", True)),
      FINALLY_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FINALLY", True)),
      FIRST_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FIRST", True)),
      FLOAT_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FLOAT", True)),
      FLOOR_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FLOOR", True)),
      FOR_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FOR", True)),
      FROM_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("FROM", True)),
      GENERIC_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("GENERIC", True)),
      IF_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("IF", True)),
      IMPORT_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("IMPORT", True)),
      IN_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("IN", True)),
      INC_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("INC", True)),
      INTEGER_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("INTEGER", True)),
      INTERFACE_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("INTERFACE", True)),
      ISTYPE_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ISTYPE", True)),
      LAST_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("LAST", True)),
      LOCK_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("LOCK", True)),
      LONGREAL_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("LONGREAL", True)),
      LOOP_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("LOOP", True)),
      LOOPHOLE_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("LOOPHOLE", True)),
      MAX_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("MAX", True)),
      METHODS_T                   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("METHODS", True)),
      MIN_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("MIN", True)),
      MOD_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("MOD", True)),
      MODULE_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("MODULE", True)),
      MUTEX_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("MUTEX", True)),
      NARROW_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NARROW", True)),
      NEW_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NEW", True)),
      NIL_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NIL", True)),
      NOT_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NOT", True)),
      NULL_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NULL", True)),
      NUMBER_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NUMBER", True)),
      OBJECT_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("OBJECT", True)),
      OF_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("OF", True)),
      OR_T                        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("OR", True)),
      ORD_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ORD", True)),
      OVERRIDES_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("OVERRIDES", True)),
      PROCEDURE_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("PROCEDURE", True)),
      RAISE_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("RAISE", True)),
      RAISES_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("RAISES", True)),
      READONLY_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("READONLY", True)),
      REAL_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("REAL", True)),
      RECORD_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("RECORD", True)),
      REF_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("REF", True)),
      REFANY_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("REFANY", True)),
      REPEAT_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("REPEAT", True)),
      RETURN_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("RETURN", True)),
      REVEAL_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("REVEAL", True)),
      ROOT_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ROOT", True)),
      ROUND_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("ROUND", True)),
      SET_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("SET", True)),
      SUBARRAY_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("SUBARRAY", True)),
      TEXT_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TEXT", True)),
      THEN_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("THEN", True)),
      TRUE_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TRUE", True)),
      TRUNC_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TRUNC", True)),
      TRY_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TRY", True)),
      TYPE_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TYPE", True)),
      TYPECASE_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TYPECASE", True)),
      TYPECODE_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TYPECODE", True)),
      UNSAFE_T                    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("UNSAFE", True)),
      UNTIL_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("UNTIL", True)),
      UNTRACED_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("UNTRACED", True)),
      VALUE_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("VALUE", True)),
      VAL_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("VAL", True)),
      VAR_T                       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("VAR", True)),
      WHILE_T                     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("WHILE", True)),
      WITH_T                      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("WITH", True)),
      Plus_T                      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+")),
      Minus_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-")),
      Splat_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("*")),
      Slant_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("/")),
      GT_T                        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<")),
      LT_T                        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">")),
      GTE_T                       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<=")),
      LTE_T                       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">=")),
      Octothorpe_T                => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("#")),
      Embrace_T                   => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("{")),
      Bracelet_T                  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("}")),
      Wax_T                       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Wane_T                      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      Bracket_T                   => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("[")),
      Unbracket_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("]")),
      Equals_T                    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=")),
      Semicolon_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),
      Spike_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("|")),
      Caret_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("^")),
      Dot_T                       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
      Range_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("..")),
      Assign_T                    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":=")),
      Comma_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      Ampersand_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("&")),
      Colon_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":")),
      Subtype_T                   => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<:")),
      Arrow_T                     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=>")),
      Integer_Number_T            => Tokenizer.Get
        (OpenToken.Recognizer.Integer.Get
           (Allow_Underscores     => False, Allow_Exponent => False,
            Allow_Signs           => False, Allow_Leading_Zero => False)),
      Real_Number_T               => Tokenizer.Get
        (OpenToken.Recognizer.Real.Get
           (Allow_Underscores     => False, Allow_Signs => False,
            Allow_Laziness        => True)),
      Character_T                 => Tokenizer.Get
        (OpenToken.Recognizer.Graphic_Character.Get
           (Exclude               => Ada.Strings.Maps.To_Set ("'\"))),
      Escape_Sequence_T           => Tokenizer.Get
        (OpenToken.Recognizer.Escape_Sequence.Get (Ada.Strings.Maps.To_Set ("tnfr""'\"))),
      Identifier_T                => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars           => Ada.Strings.Maps.Constants.Letter_Set,
            Body_Chars            => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
      Comment_T                   => Tokenizer.Get
        (OpenToken.Recognizer.Bracketed_Comment.Get
           ("(*", "*)",
            Nested                => True)),
      Octal_Escape_T              => Tokenizer.Get (OpenToken.Recognizer.Octal_Escape.Get),
      String_T                    => Tokenizer.Get
        (OpenToken.Recognizer.String.Get
           (Escapeable            => True,
            Double_Delimiter      => False,
            Escape_Mapping        => M3_Style_Escape_Code_Map)),
      Whitespace_T                => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (M3_Whitespace)),
      End_of_File_T               => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax);

end M3_Lexer;
