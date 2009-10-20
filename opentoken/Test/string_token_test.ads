--  Abstract :
--
--  Test a grammar with a string token
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
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

with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated.String_Literal;
package String_Token_Test is

   type Token_ID_Type is
     ( --  terminals
      Whitespace_ID, --  first to debug lookahead logic
      EOF_ID,
      String_ID,

      --  non-terminals
      Parse_Sequence_ID,
      Statement_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   --  Our terminal token types.
   package String_Literal is new Master_Token.String_Literal;

   --  Stuff for grammar fragments
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   --  Parser stuff.
   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => String_ID);
   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

   package Tokens is
      --  For use in right hand sides.
      String : constant Master_Token.Class := String_Literal.Get (String_ID);
      EOF    : constant Master_Token.Class := Master_Token.Get (EOF_ID);
   end Tokens;

   Syntax : constant Tokenizer.Syntax :=
     ( --  terminals
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      String_ID     => Tokenizer.Get
        (Recognizer => OpenToken.Recognizer.String.Get,
         New_Token  => Tokens.String),
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

   procedure Synthesize_Display
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type);
   --  Display string value of Source

   Display_Action : constant Nonterminal.Synthesize := Synthesize_Display'Access;

   --  valid sequence:
   --  string
   Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Statement and
     Statement      <= Tokens.String & Tokens.EOF + Display_Action;

end String_Token_Test;
