-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Selection_Mixin;
with OpenToken.Token.Sequence_Mixin;
with OpenToken.Token.List_Mixin;

-------------------------------------------------------------------------------
--  This example is a recursive-decent implementation of Example 5.10 from
--  "Compilers Principles, Techniques, and Tools" by Aho, Sethi, and Ullman
--  (aka: "The Dragon Book"). It demonstrates handling of synthesized
--  attributes.
-------------------------------------------------------------------------------
package ASU_Example_5_10_RD is

   --  The complete list of tokens, with the terminals listed first.
   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_Sign_ID,
                      Multiply_ID, EOF_ID, Whitespace_ID);

   --  Instantiate all the nessecary packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);
   package Integer_Literal is new Master_Token.Integer_Literal;

   --  Define a lexer syntax for the terminals
   Syntax : constant Tokenizer.Syntax :=
     (Multiply_ID    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("*")),
      Left_Paren_ID  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("(")),
      Right_Paren_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get (")")),
      Plus_Sign_ID   => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("+")),
      Integer_ID     => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Integer.Get
                                       (Allow_Signs => False),
                                       New_Token  => Integer_Literal.Get (Integer_ID)
                                       ),
      EOF_ID         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                       (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --------------------------------------------------------------------------
   --  Our custom token types.
   --
   --  For the most part we just build them from sequences and selections
   --  using

   --  A base token type for integer-valued tokens
   type Integer_Token is abstract new OpenToken.Token.Instance with record
      Value : Integer;
   end record;
   type Integer_Token_Handle is access all Integer_Token'Class;

   --  Create a custom selection token which has integers for components and returns
   --  an integer with the value of the selected component from a parse.
   package Integer_Selection_Token is new OpenToken.Token.Selection_Mixin (Integer_Token, OpenToken.Token.Instance);
   type Integer_Selection is new Integer_Selection_Token.Instance with null record;
   overriding procedure
     Build (Match : in out Integer_Selection;
            From  : in     OpenToken.Token.Instance'Class);
   overriding function "or"
     (Left  : access OpenToken.Token.Instance'Class;
      Right : access OpenToken.Token.Instance'Class)
     return Integer_Selection;
   overriding function "or"
     (Left  : access OpenToken.Token.Instance'Class;
      Right : in     Integer_Selection)
     return Integer_Selection;
   overriding function "or"
     (Left  : in     Integer_Selection;
      Right : access OpenToken.Token.Instance'Class)
     return Integer_Selection;
   overriding function "or"
     (Left  : in Integer_Selection;
      Right : in Integer_Selection)
     return Integer_Selection;
   type Integer_Selection_Handle is access all Integer_Selection;



   --  A token type for groupings of expressions
   package Integer_Sequence_Token is new OpenToken.Token.Sequence_Mixin (Integer_Token);
   type Expression_Sequence is new Integer_Sequence_Token.Instance with null record;
   type Expression_Sequence_Handle is access all Expression_Sequence;
   overriding procedure Build (Match : in out Expression_Sequence);
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Expression_Sequence;
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Expression_Sequence)
     return Expression_Sequence;
   overriding function "&"
     (Left  : in     Expression_Sequence;
      Right : access OpenToken.Token.Class)
     return Expression_Sequence;
   overriding function "&"
     (Left  : in Expression_Sequence;
      Right : in Expression_Sequence)
     return Expression_Sequence;

   --  Token types for mathematical operations
   package Operation_List is new OpenToken.Token.List_Mixin (Integer_Token, Integer_Token);
   type Multiply_Operation_List is new Operation_List.Instance with null record;
   overriding procedure Initialize (Match : in out Multiply_Operation_List);
   overriding procedure Add_List_Element
     (Match   : in out Multiply_Operation_List;
      Element : in out Integer_Token'Class);
   overriding function Get
     (Element   : access Integer_Token'Class;
      Separator : access OpenToken.Token.Class)
     return Multiply_Operation_List;

   type Add_Operation_List is new Operation_List.Instance with null record;
   overriding procedure Initialize (Match : in out Add_Operation_List);
   overriding procedure Add_List_Element
     (Match   : in out Add_Operation_List;
      Element : in out Integer_Token'Class);
   overriding function Get
     (Element   : access Integer_Token'Class;
      Separator : access OpenToken.Token.Class)
     return Add_Operation_List;

   type L_Sequence is new Integer_Sequence_Token.Instance with null record;
   type L_Sequence_Handle is access all L_Sequence;
   overriding procedure Build (Match : in out L_Sequence);
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return L_Sequence;
   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     L_Sequence)
     return L_Sequence;
   overriding function "&"
     (Left  : in     L_Sequence;
      Right : access OpenToken.Token.Class)
     return L_Sequence;
   overriding function "&"
     (Left  : in L_Sequence;
      Right : in L_Sequence)
     return L_Sequence;

   --  Define all our tokens
   --  ...terminals
   Times       : constant Master_Token.Handle := Syntax (Multiply_ID).Token_Handle;
   Left_Paren  : constant Master_Token.Handle := Syntax (Left_Paren_ID).Token_Handle;
   Right_Paren : constant Master_Token.Handle := Syntax (Right_Paren_ID).Token_Handle;
   Plus        : constant Master_Token.Handle := Syntax (Plus_Sign_ID).Token_Handle;
   Int_Literal : constant Master_Token.Handle := Syntax (Integer_ID).Token_Handle;
   EOF         : constant Master_Token.Handle := Syntax (EOF_ID).Token_Handle;
   --  ...and nonterminals.
   L           : constant L_Sequence_Handle        := new L_Sequence;
   E           : constant OpenToken.Token.Handle   := new Add_Operation_List;
   T           : constant Integer_Token_Handle     := new Multiply_Operation_List;
   F           : constant Integer_Selection_Handle := new Integer_Selection;

end ASU_Example_5_10_RD;
