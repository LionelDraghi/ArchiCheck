-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-- $Log: token_list_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:10:02  Ted
-- initial version
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Token.List;
with OpenToken.Text_Feeder.String;

-------------------------------------------------------------------------------
-- Test driver for the token list handling code.
-------------------------------------------------------------------------------
procedure Token_List_Test.Run is
begin

   ----------------------------------------------------------------------------
   -- Test Case 1
   --
   -- Inputs           : A valid list of tokens.
   --
   -- Expected Results : A Token.LIst
   -- Purpose          : Verify that a valid list of tokens is properly parsed.
   Test_Case_1 : declare

      Parse_String : constant String := "5, 3, 1000, 78";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      List : OpenToken.Token.List.Class :=
        OpenToken.Token.List.Get
        (Element   => Syntax(Int).Token_Handle,
         Separator => Syntax(Comma).Token_Handle
         );

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of valid token list...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Perform the parse
      OpenToken.Token.List.Parse
        (Match    => List,
         Analyzer => Analyzer
         );

      if Tokenizer.ID (Analyzer) = EOF then
         Ada.Text_IO.Put_Line ("passed");
      else
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("There was an unexpected " &
                               Token_Ids'Image (Tokenizer.ID (Analyzer)) &
                               " left on the input stream.");
      end if;

   exception
      when Error: others =>
         Ada.Text_IO.Put_Line ("failed due to parse exception:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_1;

   ----------------------------------------------------------------------------
   -- Test Case 2
   --
   -- Inputs           : A couple of integer tokens.
   --
   -- Expected Results : A Token.List
   -- Purpose          : Verify that a single token is properly parsed as a
   --                    token list.
   Test_Case_2 : declare

      Parse_String : constant String := "5 1000";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      List : OpenToken.Token.List.Class :=
        OpenToken.Token.List.Get
        (Element   => Syntax(Int).Token_Handle,
         Separator => Syntax(Comma).Token_Handle
         );

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of single-token token list...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Parse 2 token lists (one for each integer).
      OpenToken.Token.List.Parse
        (Match    => List,
         Analyzer => Analyzer
         );

      OpenToken.Token.List.Parse
        (Match    => List,
         Analyzer => Analyzer
         );

      if Tokenizer.ID (Analyzer) = EOF then
         Ada.Text_IO.Put_Line ("passed");
      else
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("There was an unexpected " &
                               Token_Ids'Image (Tokenizer.ID (Analyzer)) &
                               " left on the input stream.");
      end if;

   exception
      when Error: others =>
         Ada.Text_IO.Put_Line ("failed due to parse exception:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_2;

   ----------------------------------------------------------------------------
   -- Test Case 3
   --
   -- Inputs           : An invalid token list that ends with a comma.
   --
   -- Expected Results : A parse exception.
   -- Purpose          : Verify that an invalid token list is correctly
   --                    diagnosed.
   --
   Test_Case_3 : declare

      Parse_String : constant String := "5,1000, ";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      List : OpenToken.Token.List.Class :=
        OpenToken.Token.List.Get
        (Element   => Syntax(Int).Token_Handle,
         Separator => Syntax(Comma).Token_Handle
         );

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of invalid token list...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Parse 2 token lists (one for each integer).
      OpenToken.Token.List.Parse
        (Match    => List,
         Analyzer => Analyzer
         );

      Ada.Text_IO.Put_Line ("failed.");

   exception
      when OpenToken.Token.Parse_Error =>
        Ada.Text_IO.Put_Line ("passed.");

        when Error : others =>
         Ada.Text_IO.Put_Line ("failed due to parse exception:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_3;
end Token_List_Test.Run;
