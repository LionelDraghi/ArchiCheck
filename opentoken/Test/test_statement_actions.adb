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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR;
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
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Statement_Actions is

   type Token_ID_Type is
     (Plus_Minus_ID,
      Semicolon_ID,
      Set_ID,
      Verify_ID,
      Int_ID,
      EOF_ID,
      Whitespace_ID,

      --  non-terminals
      Value_ID,
      Statement_ID,
      Statement_Semi_ID,
      Statement_Sequence_ID,
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
      Integer        : constant Master_Token.Class := Integer_Literal.Get (Int_ID);
      Plus_Minus     : constant Master_Token.Class := Master_Token.Get (Plus_Minus_ID);
      Semicolon      : constant Master_Token.Class := Master_Token.Get (Semicolon_ID);

      --  Nonterminals
      Parse_Sequence     : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
      Statement          : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
      Statement_Semi     : constant Nonterminal.Class := Nonterminal.Get (Statement_Semi_ID);
      Statement_Sequence : constant Nonterminal.Class := Nonterminal.Get (Statement_Sequence_ID);
   end Tokens;

   package Set_Statement is

      type Instance is new Nonterminal.Instance with null record;

      Set_Statement : constant Instance := (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        Production_List.Only
        (Set_Statement <= Nonterminal.Get (Set_ID) & Integer_Literal.Get (Int_ID) + Nonterminal.Synthesize_Self);

   end Set_Statement;

   package Verify_Statement is

      type Instance is new Nonterminal.Instance with null record;

      Verify_Statement : constant Instance :=
        (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Integer_Literal.Get (Int_ID) + Nonterminal.Synthesize_Self
        and
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Integer_Literal.Get (Int_ID) &
        Tokens.Plus_Minus  + Nonterminal.Synthesize_Self;
   end Verify_Statement;

   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => Whitespace_ID);

   Syntax : constant Tokenizer.Syntax :=
     (
      --  terminals: operators etc

      Plus_Minus_ID => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+-")),
      Semicolon_ID  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

      --  terminals: keywords
      Set_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("set")),
      Verify_ID => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("verify")),

      --  terminals: values
      Int_ID  => Tokenizer.Get (OpenToken.Recognizer.Based_Integer.Get, New_Token => Tokens.Integer),

      --  Syntax only
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   --  We can't set the grammar to parse only one statement, but then
   --  pass in a string with two statements. The first parse doesn't
   --  return until it consumes the first token of the second
   --  statement; then the second parse doesn't see it. See
   --  test_lr0_kernels.adb for an example of this.
   --
   --  To get a similar effect, specify an action on a statement

   Action_Count : Integer := 0;

   procedure Statement_Action
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type)
   is
      pragma Unreferenced (To_ID);
      pragma Unreferenced (Source);
   begin
      New_Token := Nonterminal.Get (Statement_Semi_ID);
      Action_Count := Action_Count + 1;
   end Statement_Action;

   Grammar : constant Production_List.Instance :=
     Tokens.Parse_Sequence     <= Tokens.Statement_Sequence & Tokens.EOF and
     Tokens.Statement_Sequence <= Tokens.Statement_Semi & Tokens.Statement_Sequence and
     Tokens.Statement_Sequence <= Tokens.Statement_Semi and
     Tokens.Statement_Semi     <= Tokens.Statement & Tokens.Semicolon + Statement_Action'Access and

     Set_Statement.Grammar and
     Verify_Statement.Grammar;
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

   procedure Execute_Command (Command : in String)
   is
      use LALR_Parser;
   begin
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);

      Set_Text_Feeder (Command_Parser, String_Feeder'Unchecked_Access);

      --  Read and parse statements from the string until end of string
      Parse (Command_Parser);
   exception
   when E : others =>
      AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
   end Execute_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use AUnit.Assertions;
   begin
      Command_Parser := LALR_Parser.Generate (Grammar, An_Analyzer, Trace => Test.Debug);

      if Test.Debug then
         Dump_Grammar;
         Dump_Parse_Table;
         LALR_Parser.Set_Trace (Command_Parser, True);
      end if;

      Execute_Command ("set 2;");

      Assert (Action_Count = 1, "1 statement");

      Execute_Command ("set 2; verify 3;");

      Assert (Action_Count = 3, "2 more statements");

   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Statement_Actions");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Statement_Actions;
