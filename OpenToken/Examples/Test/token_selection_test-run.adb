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
-- $Log: token_selection_test-run.adb,v $
-- Revision 1.1  2000/08/12 21:05:10  Ted
-- initial version
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Token.Selection;
with OpenToken.Text_Feeder.String;

use type OpenToken.Token.Selection.Instance;

-------------------------------------------------------------------------------
-- Test driver for the token selection handling code.
-------------------------------------------------------------------------------
procedure Token_Selection_Test.Run is
begin

   ----------------------------------------------------------------------------
   -- Test Case 1
   --
   -- Inputs           : A selection token a token source with token that is in
   --                    the selection.
   --
   -- Expected Results : A Token.Selection
   -- Purpose          : Verify that a valid selection of tokens is properly
   --                    parsed.
   Test_Case_1 : declare

      Parse_String : constant String := "Do several things 200 times in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      Selection : OpenToken.Token.Selection.Class :=
	Do_Keyword or Several_Keyword or Things_Keyword or Int_Literal or Times_Keyword or 
	In_Keyword or A_Keyword or Row_Keyword;

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of valid token selection...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);
      
      for String_Token in 1..8 loop
	 -- Perform the parse
	 OpenToken.Token.Selection.Parse
	   (Match    => Selection,
	    Analyzer => Analyzer
	    );
	 
      end loop;

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
   -- Inputs           : An invalid token selection.
   --
   -- Expected Results : A parse exception.
   -- Purpose          : Verify that an invalid token selection is correctly
   --                    diagnosed.
   --
   Test_Case_2 : declare

      Parse_String : constant String := "Do several things in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'access);

      Selection : OpenToken.Token.Selection.Class :=
	Several_Keyword or Things_Keyword or Int_Literal or Times_Keyword or
	In_Keyword or A_Keyword or Row_Keyword;

      Passed : Boolean := True;

   begin

      Ada.Text_IO.Put ("Testing parsing of invalid token selection...");
      Ada.Text_IO.Flush;

      -- Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set
        (Feeder => String_Feeder,
         Value  => Parse_String
         );

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      -- Parse token selection
      OpenToken.Token.Selection.Parse
        (Match    => Selection,
         Analyzer => Analyzer
         );

      Ada.Text_IO.Put_Line ("failed.");

   exception
      when Error : OpenToken.Token.Parse_Error =>
	 Ada.Text_IO.Put_Line ("passed.");
--	 Ada.Text_IO.Put_Line ("Message was:");
--         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));

      when Error : others =>
         Ada.Text_IO.Put_Line ("failed due to parse exception:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_2;
end Token_Selection_Test.Run;
