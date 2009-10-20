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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Text_IO;
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Print;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Real;
with OpenToken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Identifier;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated.Real_Literal;
with OpenToken.Token.Enumerated.String_Literal;
package body Test_Token_Identifier_Real_String is

   type Token_ID_Type is
     (EOF_ID,
      Identifier_ID,
      Real_ID,
      String_ID,
      Whitespace_ID,

      --  non-terminals
      Value_ID,
      Parse_Sequence_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   package Identifier_Tokens is new Master_Token.Identifier;
   package Real_Tokens is new Master_Token.Real_Literal (Float);
   package String_Tokens is new Master_Token.String_Literal;

   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   package Tokens is
      --  Terminals
      EOF        : constant Master_Token.Class := Master_Token.Get (EOF_ID);
      Identifier : constant Master_Token.Class := Identifier_Tokens.Get (Identifier_ID);
      Real       : constant Master_Token.Class := Real_Tokens.Get (Real_ID);
      String     : constant Master_Token.Class := String_Tokens.Get (String_ID);

      --  Nonterminals
      Value          : constant Nonterminal.Class := Nonterminal.Get (Value_ID);
      Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   end Tokens;

   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => Whitespace_ID);

   Syntax : constant Tokenizer.Syntax :=
     (EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      Identifier_ID => Tokenizer.Get (OpenToken.Recognizer.Identifier.Get, Tokens.Identifier),
      Real_ID       => Tokenizer.Get (OpenToken.Recognizer.Real.Get, Tokens.Real),
      String_ID     => Tokenizer.Get (OpenToken.Recognizer.String.Get, Tokens.String),
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   Expected_Identifier : Ada.Strings.Unbounded.Unbounded_String;
   Expected_Real       : Float;
   Expected_String     : Ada.Strings.Unbounded.Unbounded_String;

   procedure Test_Action
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type)
   is
      use Token_List;
      use Ada.Strings.Unbounded;
      use OpenToken.Buffers;

      I : constant List_Iterator := Initial_Iterator (Source); --  identifier | real | string

   begin
      New_Token := Nonterminal.Get (To_ID);

      case Master_Token.ID (Token_Handle (I).all) is
      when Identifier_ID =>
         declare
            Identifier : Identifier_Tokens.Instance renames Identifier_Tokens.Instance (Token_Handle (I).all);
         begin
            AUnit.Assertions.Assert
              (To_String (Expected_Identifier) = To_String (Identifier.Identifier), "identifier mismatch");
         end;

      when Real_ID =>
         declare
            Real : Real_Tokens.Instance renames Real_Tokens.Instance (Token_Handle (I).all);
         begin
            --  Note that this check only works for reals with exact
            --  representations; good enough for this purpose.
            AUnit.Assertions.Assert
              (Expected_Real = Real_Tokens.Value (Real), "real mismatch");
         end;

      when String_ID =>
         declare
            String : String_Tokens.Instance renames String_Tokens.Instance (Token_Handle (I).all);
         begin
            AUnit.Assertions.Assert
              (To_String (Expected_String) = String_Tokens.Value (String),
               "string mismatch; got '" & String_Tokens.Value (String) & "'");
         end;

      when others =>
         raise Program_Error;
      end case;
   end Test_Action;

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   Grammar : constant Production_List.Instance :=
     Tokens.Parse_Sequence <= Tokens.Value and
     Tokens.Value <= Tokens.Identifier + Test_Action'Access  and
     Tokens.Value <= Tokens.Real + Test_Action'Access  and
     Tokens.Value <= Tokens.String + Test_Action'Access;

   package OpenToken_Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new OpenToken_Parser.LALR;
   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   An_Analyzer   : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   Parser        : LALR_Parser.Instance;

   procedure Print_Action (Action : in Nonterminal.Synthesize)
   is
      use type Nonterminal.Synthesize;
   begin
      if Action = null then
         Ada.Text_IO.Put ("<none>");
      elsif Action = Test_Action'Access then
         Ada.Text_IO.Put ("Test_Action");
      else
         Ada.Text_IO.Put ("?");
      end if;
   end Print_Action;


   procedure Dump_Grammar
   is
      package Print_Token_List is new Token_List.Print;
      package Print_Production is new Production.Print (Print_Token_List, Print_Action);
      package Print_Production_List is new Production_List.Print (Print_Production.Print);
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Grammar:");
      Print_Production_List.Print (Grammar);
      Ada.Text_IO.New_Line (2);
   end Dump_Grammar;

   procedure Execute (Input : in String; Trace : in Boolean)
   is begin
      if Trace then
         Ada.Text_IO.Put_Line ("parsing '" & Input & "'");
      end if;

      OpenToken.Text_Feeder.String.Set (String_Feeder, Input);

      LALR_Parser.Parse (Parser);
   end Execute;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      procedure One_Identifier (Item : in String)
      is begin
         Expected_Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Item);
         Execute (Item, Trace => Test.Debug);
      end One_Identifier;

      procedure One_Real (Item : in String)
      is begin
         Expected_Real := Float'Value (Item);
         Execute (Item, Trace => Test.Debug);
      end One_Real;

      procedure One_String (Source : in String; Expected : in String)
      is begin
         Expected_String := Ada.Strings.Unbounded.To_Unbounded_String (Expected);
         Execute (Source, Trace => Test.Debug);
      end One_String;

   begin
      --  We assume the recognizer works; just show that the
      --  identifier gets stored in the token properly.

      Parser := LALR_Parser.Generate (Grammar, An_Analyzer, Trace => Test.Debug);

      if Test.Debug then
         Dump_Grammar;
      end if;

      LALR_Parser.Set_Text_Feeder (Parser, String_Feeder'Unchecked_Access);

      LALR_Parser.Set_Trace (Parser, Test.Debug);

      One_Identifier ("An_Identifier");
      One_Identifier ("Another_Identifier");

      One_Real ("1.0");
      One_Real ("-4.5");

      One_String ("""foo""", "foo");
      One_String ("""foo""""bar""", "foo""bar");
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Token_Identifier_Real_String");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Token_Identifier_Real_String;
