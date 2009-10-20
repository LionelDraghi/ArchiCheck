--  Abstract :
--
--  Test grammar generator with an Ada-like Name syntax
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
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Identifier;
with OpenToken.Token.Enumerated.Nonterminal;
package Name_Token_Test is

   type Token_ID_Type is
     ( --  terminals
      Whitespace_ID, --  first to debug lookahead logic
      Dot_ID,
      EOF_ID,
      Paren_Left_ID,
      Paren_Right_ID,
      Identifier_ID,

      --  non-terminals
      Component_ID,
      Component_List_ID,
      Name_ID,
      Parse_Sequence_ID,
      Statement_ID,
      Symbol_Name_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   --  Our terminal token types.
   package Identifier_Token is new Master_Token.Identifier;

   --  Stuff for grammar fragments
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   --  Parser stuff.
   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => Identifier_ID);
   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

   package Tokens is
      --  For use in right hand sides, syntax.
      Dot            : constant Master_Token.Class := Master_Token.Get (Dot_ID);
      Paren_Left     : constant Master_Token.Class := Master_Token.Get (Paren_Left_ID);
      Paren_Right    : constant Master_Token.Class := Master_Token.Get (Paren_Right_ID);
      Identifier     : constant Master_Token.Class := Identifier_Token.Get (Identifier_ID);
      EOF            : constant Master_Token.Class := Master_Token.Get (EOF_ID);
   end Tokens;

   Syntax : constant Tokenizer.Syntax :=
     ( --  terminals
      Dot_ID            => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
      EOF_ID            => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      Paren_Left_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Paren_Right_ID    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      Identifier_ID => Tokenizer.Get
        (Recognizer => OpenToken.Recognizer.Identifier.Get (Body_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set),
         New_Token  => Tokens.Identifier),
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   The_Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   --  This analyzer is not actually used by the parser; a copy is made!

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   --  For use in right or left hand sides
   Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Statement      : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
   Name           : constant Nonterminal.Class := Nonterminal.Get (Name_ID);
   Symbol_Name    : constant Nonterminal.Class := Nonterminal.Get (Symbol_Name_ID);
   Component      : constant Nonterminal.Class := Nonterminal.Get (Component_ID);
   Component_List : constant Nonterminal.Class := Nonterminal.Get (Component_List_ID);

   --  valid names:
   --  Module (Index)
   --  Module.Component
   Simple_Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Statement and
     Statement      <= Name & Tokens.EOF and
     Name           <= Tokens.Identifier & Component + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Dot & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Nonterminal.Synthesize_Self and
      --  extra stuff so Generate doesn't complain
     Symbol_Name    <= Tokens.Dot and
     Component_List <= Tokens.Dot;

   --  valid names:
   --  Module.Symbol (Index)
   --  Module.Symbol.Component
   Medium_Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Statement and
     Statement      <= Name & Tokens.EOF and
     Name           <= Symbol_Name & Component + Nonterminal.Synthesize_Self and
     Symbol_Name    <= Tokens.Identifier & Tokens.Dot & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Dot & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Nonterminal.Synthesize_Self and
      --  extra stuff so Generate doesn't complain
     Component_List <= Tokens.Dot;

   --  valid names:
   --  Module.Symbol
   --  Module.Symbol (Index)
   --  Module.Symbol.Component
   --  Module.Symbol (Index).Component
   --  Module.Symbol.Component (Index) ...
   Full_Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Statement and
     Statement      <= Name & Tokens.EOF and
     Name           <= Symbol_Name & Component_List + Nonterminal.Synthesize_Self and
     Name           <= Symbol_Name + Nonterminal.Synthesize_Self and
     Symbol_Name    <= Tokens.Identifier & Tokens.Dot & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Component_List <= Component & Component_List + Nonterminal.Synthesize_Self and
     Component_List <= Component + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Dot & Tokens.Identifier + Nonterminal.Synthesize_Self and
     Component      <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Nonterminal.Synthesize_Self;

end Name_Token_Test;
