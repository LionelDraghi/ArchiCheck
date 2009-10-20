--  Abstract :
--
--  Test grammar generator with an Ada-like association syntax. In an
--  earlier version of OpenToken, this grammar reported spurious
--  conflicts.
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Ada.Strings.Maps.Constants;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Identifier;
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Token.Enumerated.Nonterminal;
package Association_Token_Test is

   type Token_ID_Type is
     ( --  terminals
      Whitespace_ID, --  first to debug lookahead logic

      Comma_ID,
      Equals_Greater_ID,
      Identifier_ID,
      Int_ID,
      Paren_Left_ID,
      Paren_Right_ID,

      --  last terminal
      EOF_ID,

      --  non-terminals
      Aggregate_ID,
      Association_ID,
      Association_List_ID,
      Parse_Sequence_ID,
      Statement_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   package Identifier_Token is new Master_Token.Identifier;
   package Integer_Literal is new Master_Token.Integer_Literal;

   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => EOF_ID);
   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

   package Tokens is
      --  For use in right hand sides, syntax.
      Comma          : constant Master_Token.Class := Master_Token.Get (Comma_ID);
      EOF            : constant Master_Token.Class := Master_Token.Get (EOF_ID);
      Equals_Greater : constant Master_Token.Class := Master_Token.Get (Equals_Greater_ID);
      Identifier     : constant Master_Token.Class := Identifier_Token.Get (Identifier_ID);
      Integer        : constant Master_Token.Class := Integer_Literal.Get (Int_ID);
      Paren_Left     : constant Master_Token.Class := Master_Token.Get (Paren_Left_ID);
      Paren_Right    : constant Master_Token.Class := Master_Token.Get (Paren_Right_ID);
   end Tokens;

   Syntax : constant Tokenizer.Syntax :=
     ( --  terminals
      Whitespace_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Comma_ID          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      Equals_Greater_ID => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=>")),
      Int_ID            => Tokenizer.Get
        (OpenToken.Recognizer.Integer.Get,
         New_Token      => Integer_Literal.Get (Int_ID)),
      Identifier_ID     => Tokenizer.Get
        (Recognizer     => OpenToken.Recognizer.Identifier.Get
           (Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
         New_Token      => Tokens.Identifier),
      Paren_Left_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Paren_Right_ID    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      EOF_ID            => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF)
     );

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   The_Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   --  This analyzer is not actually used by the parser; a copy is made!

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   --  For use in right or left hand sides
   Aggregate        : constant Nonterminal.Class := Nonterminal.Get (Aggregate_ID);
   Association      : constant Nonterminal.Class := Nonterminal.Get (Association_ID);
   Association_List : constant Nonterminal.Class := Nonterminal.Get (Association_List_ID);
   Parse_Sequence   : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Statement        : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);

   --  valid syntax:
   --  (identifier)
   --  (identifier, identifier)
   --  (identifier => identifier)
   --  (integer => identifier)
   --  (identifier => identifier, integer => identifier)
   Full_Grammar : constant Production_List.Instance :=
     Parse_Sequence   <= Statement and
     Statement        <= Aggregate & Tokens.EOF and
     Aggregate        <= Tokens.Paren_Left & Association_List & Tokens.Paren_Right + Nonterminal.Synthesize_Self and
     Association_List <= Association & Tokens.Comma & Association_List + Nonterminal.Synthesize_Self and
     Association_List <= Association + Nonterminal.Synthesize_Self and
     Association      <= Tokens.Identifier & Tokens.Equals_Greater & Tokens.Identifier +
     Nonterminal.Synthesize_Self and
     Association      <= Tokens.Integer & Tokens.Equals_Greater & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Association      <= Tokens.Identifier + Nonterminal.Synthesize_Self;

end Association_Token_Test;
