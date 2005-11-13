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
-- $Log: token_sequence_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:00:48  Ted
-- initial version
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Token.Sequence;
with OpenToken.Text_Feeder.String;

use type OpenToken.Token.Sequence.Instance;

-------------------------------------------------------------------------------
-- Test driver for the token sequence handling code.
-------------------------------------------------------------------------------
procedure Token_Sequence_Test.Run is
begin

   ----------------------------------------------------------------------------
   -- Test Case 1
   --
   -- Inputs           : A valid sequence of tokens.
   --
   -- Expected Results : A Token.Sequence
   -- Purpose          : Verify that a valid sequence of tokens is properly parsed.
   Test_Case_1 : declare

      Parse_String : constant String := "Do several things 200 times in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      Sequence : OpenToken.Token.Sequence.Class :=
        Do_Keyword & Several_Keyword & Things_Keyword &
        Int_Literal & Times_Keyword &
        In_Keyword & A_Keyword & Row_Keyword;

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of valid token sequence...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Perform the parse
      OpenToken.Token.Sequence.Parse
        (Match    => Sequence,
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
   -- Inputs           : An invalid token sequence.
   --
   -- Expected Results : A parse exception.
   -- Purpose          : Verify that an invalid token sequence is correctly
   --                    diagnosed.
   --
   Test_Case_2 : declare

      Parse_String : constant String := "Do several things in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      Sequence : OpenToken.Token.Sequence.Class :=
        Do_Keyword & Several_Keyword & Things_Keyword &
        Int_Literal & Times_Keyword & In_Keyword &
        A_Keyword & Row_Keyword;

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of invalid token sequence...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Parse token sequence
      OpenToken.Token.Sequence.Parse
        (Match    => Sequence,
         Analyzer => Analyzer
         );

      Ada.Text_IO.Put_Line ("failed.");

   exception
      when OpenToken.Token.Parse_Error =>
        Ada.Text_IO.Put_Line ("passed.");

        when Error : others =>
         Ada.Text_IO.Put_Line ("failed due to parse exception:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_2;
end Token_Sequence_Test.Run;
