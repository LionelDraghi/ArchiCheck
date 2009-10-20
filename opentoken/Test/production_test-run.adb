-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000,2009 Ted Dennison
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
--
-------------------------------------------------------------------------------

--  Test driver for the token list handling code.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Real;
with OpenToken.Recognizer.String;
procedure Production_Test.Run is
begin

   ----------------------------------------------------------------------------
   --  Test Case 1
   --
   --  Inputs           :
   --
   --  Expected Results :
   --  Purpose          :
   --
   Test_Case_1 : declare
      use type Token_List.Instance;
      use type Production.Right_Hand_Side;
      use type Production.Instance;
      use type Production_List.Instance;

      Syntax : constant Tokenizer.Syntax :=
        (Int_ID        => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
         Real_ID       => Tokenizer.Get (OpenToken.Recognizer.Real.Get),
         String_ID     => Tokenizer.Get (OpenToken.Recognizer.String.Get),
         Keyword_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("whatever"))
         );

      Int     : constant Master_Token.Handle := Syntax (Int_ID).Token_Handle;
      Keyword : constant Master_Token.Handle := Syntax (Keyword_ID).Token_Handle;
      String  : constant Master_Token.Handle := Syntax (String_ID).Token_Handle;

      Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);
      pragma Unreferenced (Analyzer);

      Expression : constant Nonterminal.Handle := new Nonterminal.Instance;
      Literal    : constant Nonterminal.Handle := new Nonterminal.Instance;

      Whatever_Production : constant Production.Instance :=
        Expression <= Int.all & Keyword.all & Int.all +
        Nonterminal.Synthesize'(Nonterminal.Synthesize_Self);

      STR_Key_Production : constant Production.Instance := Literal <= String.all & Keyword.all +
        Nonterminal.Synthesize_Self;

      Passed : Boolean := True;

      Iterator : Production_List.List_Iterator;
      List     : Production_List.Instance;
   begin

      Ada.Text_IO.Put ("Testing construction and traversal of production lists...");
      Ada.Text_IO.Flush;

      List := Whatever_Production and STR_Key_Production;
      Iterator := Production_List.Initial_Iterator (List);

      if Production_List.Get_Production (Iterator) /= Whatever_Production then
         Ada.Text_IO.Put_Line ("failed!");
         Ada.Text_IO.Put_Line
           ("  (got an unexpected production on the first try)");
         Passed := False;
      end if;
      Production_List.Next_Production (Iterator);

      if Production_List.Get_Production (Iterator) /= STR_Key_Production then
         Ada.Text_IO.Put_Line ("failed!");
         Ada.Text_IO.Put_Line
           ("  (got an unexpected production on the second try)");
         Passed := False;
      end if;
      Production_List.Next_Production (Iterator);

      if not Production_List.Last_Production (Iterator) then
         Ada.Text_IO.Put_Line ("failed!");
         Ada.Text_IO.Put_Line
           ("  (unexpected production entries at end of list)");
         Passed := False;
      end if;

      Production_List.Clean (List);

      if Passed then
         Ada.Text_IO.Put_Line ("passed");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Test_Case_1;
end Production_Test.Run;
