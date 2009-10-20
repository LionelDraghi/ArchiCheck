--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Test_Cases.Registration;
with Ada.Text_IO;
with OpenToken.Production.List.Print;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Parser;
with OpenToken.Production.Print;
with OpenToken.Recognizer.Based_Integer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_LR0_Kernels is

   --  A simple grammar that exersizes the "already in goto_set" branches in LR0_Kernels

   --  Even though EOF is not part of the grammar, it must be part of
   --  the syntax; the string_feeder inserts it to mark the end of the
   --  string.
   type Token_ID_Type is
     (EOF_ID,
      Equals_ID,
      Equals_Greater_ID,
      Paren_Left_ID,
      Plus_Minus_ID,
      Semicolon_ID,
      Set_ID,
      Verify_ID,
      Int_ID,
      Whitespace_ID,

      --  non-terminals
      Association_ID,
      Value_ID,
      Statement_ID,
      Statement_Semi_ID,
      Parse_Sequence_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   package Integer_Literal is new Master_Token.Integer_Literal;

   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   package Tokens is
      --  For use in right hand sides.
      --  Terminals
      EOF            : constant Master_Token.Class := Master_Token.Get (EOF_ID);
      Equals_Greater : constant Master_Token.Class := Master_Token.Get (Equals_Greater_ID);
      Integer        : constant Master_Token.Class := Integer_Literal.Get (Int_ID);
      Paren_Left     : constant Master_Token.Class := Master_Token.Get (Paren_Left_ID);
      Plus_Minus     : constant Master_Token.Class := Master_Token.Get (Plus_Minus_ID);
      Semicolon      : constant Master_Token.Class := Master_Token.Get (Semicolon_ID);

      --  Nonterminals
      Association    : constant Nonterminal.Class := Nonterminal.Get (Association_ID);
      Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
      Statement      : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
      Statement_Semi : constant Nonterminal.Class := Nonterminal.Get (Statement_Semi_ID);
      Value          : constant Nonterminal.Class := Nonterminal.Get (Value_ID);
   end Tokens;

   package Aggregate_Token is

      type Instance is new Nonterminal.Instance with null record;

      Aggregate : constant Instance :=
        (Master_Token.Instance (Master_Token.Get (Value_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        Production_List.Only (Aggregate <= Tokens.Paren_Left & Tokens.Association + Nonterminal.Synthesize_Self);

   end Aggregate_Token;

   package Association_Token is

      type Instance is new Nonterminal.Instance with null record;

      Association : constant Instance :=
        (Master_Token.Instance (Master_Token.Get (Association_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        --  Not a real aggregate; simpler to simplify test
        Association <= Tokens.Integer & Tokens.Equals_Greater + Nonterminal.Synthesize_Self and
        Association <= Tokens.Value + Nonterminal.Synthesize_Self;

   end Association_Token;

   package Integer_Token is

      type Instance is new Nonterminal.Instance with null record;

      Value : constant Instance := (Master_Token.Instance (Master_Token.Get (Value_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        Production_List.Only (Value <= Integer_Literal.Get (Int_ID) + Nonterminal.Synthesize_Self);

   end Integer_Token;

   package Set_Statement is

      type Instance is new Nonterminal.Instance with null record;

      Set_Statement : constant Instance := (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        Production_List.Only
        (Set_Statement <= Nonterminal.Get (Set_ID) & Tokens.Value  + Nonterminal.Synthesize_Self);

   end Set_Statement;

   package Verify_Statement is

      type Instance is new Nonterminal.Instance with null record;

      --  This produces the kernels:
      --  STATEMENT_ID(VERIFY_AGGREGATE.NONTERMINAL.INSTANCE) <= VERIFY_ID ^ VALUE_ID PLUS_MINUS_ID
      --  STATEMENT_ID(VERIFY_AGGREGATE.NONTERMINAL.INSTANCE) <= VERIFY_ID ^ VALUE_ID
      --
      --  which generates a goto_set of:
      --  STATEMENT_ID(VERIFY_AGGREGATE.NONTERMINAL.INSTANCE) <= VERIFY_ID ^ VALUE_ID
      --  STATEMENT_ID(VERIFY_AGGREGATE.NONTERMINAL.INSTANCE) <= VERIFY_ID ^ VALUE_ID
      --
      --  which is filtered by the second "already in goto_set" check.

      Verify_Statement : constant Instance :=
        (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        --  verify symbol = value;
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Tokens.Value + Nonterminal.Synthesize_Self
        and
        --  verify symbol = value +- tolerance;
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Tokens.Value &
        Tokens.Plus_Minus  + Nonterminal.Synthesize_Self;
   end Verify_Statement;

   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => Whitespace_ID);

   Syntax : constant Tokenizer.Syntax :=
     (
      --  terminals: operators etc

      EOF_ID            => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      Equals_ID         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=")),
      Equals_Greater_ID => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=>")),
      Paren_Left_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Plus_Minus_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+-")),
      Semicolon_ID      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

      --  terminals: keywords
      Set_ID          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("set")),
      Verify_ID       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("verify")),

      --  terminals: values
      Int_ID  => Tokenizer.Get (OpenToken.Recognizer.Based_Integer.Get, New_Token => Tokens.Integer),

      --  The mere presence of the Whitespace_ID token in the syntax
      --  allows whitespace in any statement, even though it is not
      --  referenced anywhere else.
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   Grammar : constant Production_List.Instance :=
     Tokens.Parse_Sequence <= Tokens.Statement_Semi and
     Tokens.Statement_Semi <= Tokens.Statement & Tokens.Semicolon and

     Set_Statement.Grammar and
     Verify_Statement.Grammar and
     Integer_Token.Grammar and
     Aggregate_Token.Grammar and
     Association_Token.Grammar;
   package OpenToken_Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new OpenToken_Parser.LALR;
   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   An_Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   Command_Parser : LALR_Parser.Instance;

   procedure Print_Action (Action : in Nonterminal.Synthesize) is null;

   procedure Dump_Grammar
   is
      package Print_Token_List is new Token_List.Print;
      package Print_Production is new Production.Print (Print_Token_List, Print_Action);
      package Print_Production_List is new Production_List.Print (Print_Production.Print);
   begin
      Print_Production_List.Print (Grammar);
   end Dump_Grammar;

   procedure Dump_Parse_Table
   is begin
      LALR_Parser.Print_Table (Command_Parser);
   end Dump_Parse_Table;

   procedure Execute_Command (Command : in String; Trace : in Boolean)
   is
      use LALR_Parser;
   begin
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);

      Set_Text_Feeder (Command_Parser, String_Feeder'Unchecked_Access);

      --  Read and parse statements from the string until end of string
      loop
         exit when End_Of_Text (Command_Parser);

         Parse (Command_Parser);

         if Trace then
            Ada.Text_IO.Put_Line ("finished one parse");
         end if;
      end loop;
   end Execute_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

   begin
      Command_Parser := LALR_Parser.Generate (Grammar, An_Analyzer, Trace => Test.Debug);

      if Test.Debug then
         Dump_Grammar;
         Dump_Parse_Table;
         LALR_Parser.Set_Trace (Command_Parser, True);
      end if;

      --  The test is that we get no exceptions. Note that the
      --  aggregate is not a real aggregate; simpler syntax for this
      --  test.
      Execute_Command ("set (2 =>;", Trace => Test.Debug);

      --  IMPROVEME: Two successive statements do _not_ work; first
      --  call to Parse eats first token of second statement.
      --  Execute_Command ("set (2 =>; set 3;", Trace => Test.Debug);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_LR0_Kernels");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR0_Kernels;
