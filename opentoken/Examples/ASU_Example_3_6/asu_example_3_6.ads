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
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Real;
with OpenToken.Recognizer.Character_Set;

with Relop_Example_Token;

---------------------------------------------------------------------------
--  This example is an implementation of Example 3.6 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book").
---------------------------------------------------------------------------
package Asu_Example_3_6 is

   type Example_Token_ID is (If_ID, Then_ID, Else_ID, ID_ID, Int, Real, Relop, Whitespace);

   package Example_Token is new OpenToken.Token.Enumerated (Example_Token_ID);
   package Tokenizer is new Example_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (If_ID   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("if")),
      Then_ID => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("then")),
      Else_ID => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("else")),
      ID_ID   => Tokenizer.Get (OpenToken.Recognizer.Identifier.Get),
      Int     => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
      Real    => Tokenizer.Get (OpenToken.Recognizer.Real.Get),
      Relop   => Tokenizer.Get (Relop_Example_Token.Get),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );


end Asu_Example_3_6;
