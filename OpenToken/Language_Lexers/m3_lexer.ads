-------------------------------------------------------------------------------
--
--  Copyright (C) 2000 David Starner
--
--  Based off java_lexer.adb
--  Copyright (C) 1999 Christoph Karl Walter Grein
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it and/or
--  modify it under the terms of the  GNU General Public License as published
--  by the Free Software Foundation; either version 2, or (at your option)
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
--
--  Maintainer: David Starner <dstarner98@aasaa.ofe.org>
--
--  Update History:
-- $Log: m3_lexer.ads,v $
-- Revision 1.2  2000/08/06 23:34:51  Ted
-- Change to work w/ new package hierarchy.
--
-- Revision 1.1  2000/02/05 04:07:18  Ted
-- Language lexer for Modula-3.
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;
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
with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.End_Of_File;

pragma Elaborate_All (Opentoken.Token.Enumerated.Analyzer,
                      OpenToken.Recognizer.Keyword, OpenToken.Recognizer.Separator,
                      OpenToken.Recognizer.Identifier,
                      OpenToken.Recognizer.Graphic_Character,
                      OpenToken.Recognizer.Escape_Sequence,
                      OpenToken.Recognizer.Octal_Escape, OpenToken.Recognizer.String,
                      OpenToken.Recognizer.Integer,
                      OpenToken.Recognizer.Real,
                      OpenToken.Recognizer.Character_Set,
                      OpenToken.Recognizer.Line_Comment, OpenToken.Recognizer.Bracketed_Comment,
                      OpenToken.Recognizer.End_Of_File);

-------------------------------------------------------------------------------
-- This is a lexer for the Modula 3 language, as defined by the Modula 3
-- standard at http://www.research.digital.com/SRC/m3defn/html/complete.html.
-- It's a work in process, and is designed as the lexer for a m3toAda
-- translator.
-------------------------------------------------------------------------------
package M3_Lexer is

   type M3_Token is (
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
--      Based_Integer_T,     -- 12_AAA0
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

   package Master_Token is new OpenToken.Token.Enumerated (M3_Token);
   package Tokenizer is new Master_Token.Analyzer;

   M3_Style_Escape_Code_Map : constant Ada.Strings.Maps.Character_Mapping :=
      Ada.Strings.Maps.To_Mapping
      (From => "tnfr""'\",
       To => Ada.Characters.Latin_1.HT &
         Ada.Characters.Latin_1.LF &
         Ada.Characters.Latin_1.FF &
         Ada.Characters.Latin_1.CR & '"' & ''' & '\');

   M3_Whitespace : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (
         Ada.Characters.Latin_1.Space &
         Ada.Characters.Latin_1.HT &
         Ada.Characters.Latin_1.VT &
         Ada.Characters.Latin_1.FF &
         OpenToken.EOL_Character);

   Syntax : constant Tokenizer.Syntax :=
      (ABS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ABS", Case_Sensitive => True)),
      ADDRESS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ADDRESS", Case_Sensitive => True)),
      ADR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ADR", Case_Sensitive => True)),
      ADRSIZE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ADRSIZE", Case_Sensitive => True)),
      AND_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("AND", Case_Sensitive => True)),
      ANY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ANY", Case_Sensitive => True)),
      ARRAY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ARRAY", Case_Sensitive => True)),
      AS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("AS", Case_Sensitive => True)),
      BEGIN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BEGIN", Case_Sensitive => True)),
      BITS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BITS", Case_Sensitive => True)),
      BITSIZE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BITSIZE", Case_Sensitive => True)),
      BOOLEAN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BOOLEAN", Case_Sensitive => True)),
      BRANDED_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BRANDED", Case_Sensitive => True)),
      BY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BY", Case_Sensitive => True)),
      BYTESIZE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("BYTESIZE", Case_Sensitive => True)),
      CARDINAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CARDINAL", Case_Sensitive => True)),
      CASE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CASE", Case_Sensitive => True)),
      CEILING_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CEILING", Case_Sensitive => True)),
      CHAR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CHAR", Case_Sensitive => True)),
      CONST_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CONST", Case_Sensitive => True)),
      DEC_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("DEC", Case_Sensitive => True)),
      DISPOSE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("DISPOSE", Case_Sensitive => True)),
      DIV_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("DIV", Case_Sensitive => True)),
      DO_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("DO", Case_Sensitive => True)),
      ELSE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ELSE", Case_Sensitive => True)),
      ELSIF_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ELSIF", Case_Sensitive => True)),
      END_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("END", Case_Sensitive => True)),
      EVAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EVAL", Case_Sensitive => True)),
      EXCEPT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EXCEPT", Case_Sensitive => True)),
      EXCEPTION_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EXCEPTION", Case_Sensitive => True)),
      EXIT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EXIT", Case_Sensitive => True)),
      EXPORTS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EXPORTS", Case_Sensitive => True)),
      EXTENDED_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("EXTENDED", Case_Sensitive => True)),
      FALSE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FALSE", Case_Sensitive => True)),
      FINALLY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FINALLY", Case_Sensitive => True)),
      FIRST_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FIRST", Case_Sensitive => True)),
      FLOAT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FLOAT", Case_Sensitive => True)),
      FLOOR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FLOOR", Case_Sensitive => True)),
      FOR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FOR", Case_Sensitive => True)),
      FROM_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("FROM", Case_Sensitive => True)),
      GENERIC_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("GENERIC", Case_Sensitive => True)),
      IF_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("IF", Case_Sensitive => True)),
      IMPORT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("IMPORT", Case_Sensitive => True)),
      IN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("IN", Case_Sensitive => True)),
      INC_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("INC", Case_Sensitive => True)),
      INTEGER_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("INTEGER", Case_Sensitive => True)),
      INTERFACE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("INTERFACE", Case_Sensitive => True)),
      ISTYPE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ISTYPE", Case_Sensitive => True)),
      LAST_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("LAST", Case_Sensitive => True)),
      LOCK_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("LOCK", Case_Sensitive => True)),
      LONGREAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("LONGREAL", Case_Sensitive => True)),
      LOOP_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("LOOP", Case_Sensitive => True)),
      LOOPHOLE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("LOOPHOLE", Case_Sensitive => True)),
      MAX_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("MAX", Case_Sensitive => True)),
      METHODS_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("METHODS", Case_Sensitive => True)),
      MIN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("MIN", Case_Sensitive => True)),
      MOD_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("MOD", Case_Sensitive => True)),
      MODULE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("MODULE", Case_Sensitive => True)),
      MUTEX_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("MUTEX", Case_Sensitive => True)),
      NARROW_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NARROW", Case_Sensitive => True)),
      NEW_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NEW", Case_Sensitive => True)),
      NIL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NIL", Case_Sensitive => True)),
      NOT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NOT", Case_Sensitive => True)),
      NULL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NULL", Case_Sensitive => True)),
      NUMBER_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NUMBER", Case_Sensitive => True)),
      OBJECT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("OBJECT", Case_Sensitive => True)),
      OF_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("OF", Case_Sensitive => True)),
      OR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("OR", Case_Sensitive => True)),
      ORD_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ORD", Case_Sensitive => True)),
      OVERRIDES_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("OVERRIDES", Case_Sensitive => True)),
      PROCEDURE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("PROCEDURE", Case_Sensitive => True)),
      RAISE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("RAISE", Case_Sensitive => True)),
      RAISES_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("RAISES", Case_Sensitive => True)),
      READONLY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("READONLY", Case_Sensitive => True)),
      REAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("REAL", Case_Sensitive => True)),
      RECORD_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("RECORD", Case_Sensitive => True)),
      REF_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("REF", Case_Sensitive => True)),
      REFANY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("REFANY", Case_Sensitive => True)),
      REPEAT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("REPEAT", Case_Sensitive => True)),
      RETURN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("RETURN", Case_Sensitive => True)),
      REVEAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("REVEAL", Case_Sensitive => True)),
      ROOT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ROOT", Case_Sensitive => True)),
      ROUND_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("ROUND", Case_Sensitive => True)),
      SET_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("SET", Case_Sensitive => True)),
      SUBARRAY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("SUBARRAY", Case_Sensitive => True)),
      TEXT_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TEXT", Case_Sensitive => True)),
      THEN_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("THEN", Case_Sensitive => True)),
      TRUE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TRUE", Case_Sensitive => True)),
      TRUNC_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TRUNC", Case_Sensitive => True)),
      TRY_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TRY", Case_Sensitive => True)),
      TYPE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TYPE", Case_Sensitive => True)),
      TYPECASE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TYPECASE", Case_Sensitive => True)),
      TYPECODE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TYPECODE", Case_Sensitive => True)),
      UNSAFE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("UNSAFE", Case_Sensitive => True)),
      UNTIL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("UNTIL", Case_Sensitive => True)),
      UNTRACED_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("UNTRACED", Case_Sensitive => True)),
      VALUE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("VALUE", Case_Sensitive => True)),
      VAL_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("VAL", Case_Sensitive => True)),
      VAR_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("VAR", Case_Sensitive => True)),
      WHILE_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("WHILE", Case_Sensitive => True)),
      WITH_T => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("WITH", Case_Sensitive => True)),
      Plus_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("+")),
      Minus_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("-")),
      Splat_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("*")),
      Slant_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("/")),
      GT_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("<")),
      LT_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (">")),
      GTE_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("<=")),
      LTE_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (">=")),
      Octothorpe_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("#")),
      Embrace_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("{")),
      Bracelet_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("}")),
      Wax_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("(")),
      Wane_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (")")),
      Bracket_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("[")),
      Unbracket_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("]")),
      Equals_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("=")),
      Semicolon_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (";")),
      Spike_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("|")),
      Caret_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("^")),
      Dot_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (".")),
      Range_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("..")),
      Assign_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (":=")),
      Comma_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (",")),
      Ampersand_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("&")),
      Colon_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (":")),
      Subtype_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("<:")),
      Arrow_T => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("=>")),
      Integer_Number_T => Tokenizer.Get(OpenToken.Recognizer.Integer.Get
         (Allow_Underscores => False, Allow_Exponent => False,
          Allow_Signs => False, Allow_Leading_Zero => False)),
--      Based_Integer_T =>  Tokenizer.Get (OpenToken.Recognizer.Based_Integer_M3_Style.Get),
      Real_Number_T => Tokenizer.Get(OpenToken.Recognizer.Real.Get
         (Allow_Underscores => False, Allow_Signs => False,
          Allow_Laziness => True)),
      Character_T => Tokenizer.Get(OpenToken.Recognizer.Graphic_Character.Get
          (Exclude => Ada.Strings.Maps.To_Set ("'\"))),
      Escape_Sequence_T => Tokenizer.Get(
         OpenToken.Recognizer.Escape_Sequence.Get (Ada.Strings.Maps.To_Set ("tnfr""'\"))),
      Identifier_T => Tokenizer.Get(OpenToken.Recognizer.Identifier.Get),
      Comment_T  => Tokenizer.Get(OpenToken.Recognizer.Bracketed_Comment.Get ("(*", "*)", Nested => True)),
      Octal_Escape_T => Tokenizer.Get (OpenToken.Recognizer.Octal_Escape.Get),
      String_T => Tokenizer.Get(OpenToken.Recognizer.String.Get
         (Escapeable => True, Double_Delimiter => False,
          Escape_Mapping => M3_Style_Escape_Code_Map)),
     Whitespace_T  => Tokenizer.Get(OpenToken.Recognizer.Character_Set.Get (M3_Whitespace)),
     End_Of_File_T => Tokenizer.Get(OpenToken.Recognizer.End_Of_File.Get));

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);

end M3_Lexer;
