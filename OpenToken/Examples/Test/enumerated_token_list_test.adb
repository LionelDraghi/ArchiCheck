-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: enumerated_token_list_test.adb,v $
-- Revision 1.1  2000/08/12 21:24:57  Ted
-- moved from token_list_test
--
-- Revision 1.1  2000/01/27 21:06:23  Ted
-- A test to verify that token lists a handled correctly.
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;
with OpenToken.Token;
with OpenToken.Token.List;
with OpenToken.Token.Analyzer;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.String;
with OpenToken.Recognizer.Real;

-------------------------------------------------------------------------------
-- Test driver for the token list handling code.
-------------------------------------------------------------------------------
procedure Enumerated_Token_List_Test is
begin

   ----------------------------------------------------------------------------
   -- Test Case 1
   --
   -- Inputs           : A series of 4 tokens catenated into a list
   --
   -- Expected Results : A list of tokens
   -- Purpose          : Verify that a list of tokens is properly created
   --                    and can be properly traversed.
   Test_Case_1 : declare
      type Token_IDs is (Int, Real, String, Keyword);

      package Master_Token is new OpenToken.Token(Token_IDs);
      package Tokenizer is new Master_Token.Analyzer;
      package Token_List is new Master_Token.List;

      use type Token_List.Instance;

      Syntax : constant Tokenizer.Syntax :=
        (Int     => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
         Real    => Tokenizer.Get (OpenToken.Recognizer.Real.Get),
         String  => Tokenizer.Get (OpenToken.Recognizer.String.Get),
         Keyword => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("whatever"))
         );

      Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);

      List : Token_List.Instance;
      Iterator : Token_List.List_Iterator;
      Passed : Boolean := True;
   begin

      Ada.Text_IO.Put ("Testing construction and traversal of token lists...");
      Ada.Text_IO.Flush;

      List := Syntax(Keyword).Token_Handle.all & Syntax(String).Token_Handle.all &
        Syntax(Real).Token_Handle.all & Syntax(Int).Token_Handle.all;
      Iterator := Token_List.Initial_Iterator (List);
      for ID in reverse Syntax'Range loop
         if Master_Token.ID (Token_List.Token_Handle(Iterator).all) /= ID then
            Ada.Text_IO.Put_Line ("failed!");
            Ada.Text_IO.Put_Line
              ("  (got a " &
               Token_IDs'Image(Master_Token.ID (Token_List.Token_Handle(Iterator).all)) &
               " where a "& Token_IDs'Image(ID) & " was expected.");
            Passed := False;
         end if;
         Token_List.Next_Token (Iterator);
      end loop;
      Token_List.Clean (List);

      if Passed then
         Ada.Text_IO.Put_Line ("passed");
      end if;
   end Test_Case_1;
end Enumerated_Token_List_Test;
