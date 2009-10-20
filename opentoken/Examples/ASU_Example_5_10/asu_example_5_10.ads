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

with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Parser.LRk_Item;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Text_Feeder.String;

with Simple_Integer_Token;

-------------------------------------------------------------------------------
--  This example is an implementation of Example 5.10 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). It demonstrates handling of synthesized attributes
-------------------------------------------------------------------------------
package ASU_Example_5_10 is

   --  The complete list of tokens, with the terminals listed first.
   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_Sign_ID,
                      Multiply_ID, EOF_ID, Whitespace_ID, L_ID, E_ID, T_ID, F_ID);

   --  Instantiate all the nessecary packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;
   package LRK_Item is new Parser.LRK_Item (1);

   --  Instantiate our tokens
   package Integer_Literal is new Master_Token.Integer_Literal;
   package Simple_Integer is new Simple_Integer_Token (Master_Token, Token_List, Nonterminal, Integer_Literal);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   --  Define all our tokens
   Times       : aliased Master_Token.Class := Master_Token.Get (Multiply_ID);
   Left_Paren  : aliased Master_Token.Class := Master_Token.Get (Left_Paren_ID);
   Right_Paren : aliased Master_Token.Class := Master_Token.Get (Right_Paren_ID);
   Plus        : aliased Master_Token.Class := Master_Token.Get (Plus_Sign_ID);
   Int_Literal : aliased Master_Token.Class := Integer_Literal.Get (Integer_ID);
   EOF         : aliased Master_Token.Class := Master_Token.Get (EOF_ID);
   L           : aliased Simple_Integer.Class := Simple_Integer.Get (L_ID);
   E           : aliased Simple_Integer.Class := Simple_Integer.Get (E_ID);
   T           : aliased Simple_Integer.Class := Simple_Integer.Get (T_ID);
   F           : aliased Simple_Integer.Class := Simple_Integer.Get (F_ID);

   --  Define a lexer syntax for the terminals
   Syntax : constant Tokenizer.Syntax :=
     (Multiply_ID    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("*"),
                                       New_Token  => Times),
      Left_Paren_ID  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("("),
                                       New_Token  => Left_Paren),
      Right_Paren_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get (")"),
                                       New_Token  => Right_Paren
                                       ),
      Plus_Sign_ID   => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("+"),
                                       New_Token  => Plus
                                       ),
      Integer_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Integer.Get
                                                   (Allow_Signs => False),
                                   New_Token  => Int_Literal
                                   ),
      EOF_ID        => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get,
                                      New_Token  => EOF
                                      ),
      Whitespace_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                      (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --------------------------------------------------------------------------
   --  Define the Grammar. The text in the example in the book looks something
   --  like:
   --
   --  L -> E         print (L.val)
   --  E -> E + T     E.val := E1.val + T.val
   --  E -> T
   --  T -> T * F     T.val := T1.val * F.val
   --  T -> F
   --  F -> ( E )     F.val := E.val
   --  F -> digit
   --
   Grammar : constant Production_List.Instance :=
     L <= E & EOF                      + Simple_Integer.Print_Value'Access and
     E <= E & Plus & T                 + Simple_Integer.Add_Integers       and
     E <= T                                                                and
     T <= T & Times & F                + Simple_Integer.Multiply_Integers  and
     T <= F                                                                and
     F <= Left_Paren & E & Right_Paren + Simple_Integer.Synthesize_Second  and
     F <= Int_Literal;

end ASU_Example_5_10;

