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
-- $Log: asu_example_4_46_rd-run.adb,v $
-- Revision 1.2  2000/08/14 03:02:35  Ted
-- Changed comment to reflect fact that gnat 3.13p is still broken
--
-- Revision 1.1  2000/08/12 16:08:47  Ted
-- Initial version
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Exceptions;

with OpenToken.Token.Sequence;
with OpenToken.Token.Selection;

use type OpenToken.Token.Sequence.Instance;
use type OpenToken.Token.Selection.Instance;

-------------------------------------------------------------------------------
-- This example is an implementation of Example 4.46 from "Compilers
-- Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
-- Dragon Book"). The example was meant to demonstrate basic LALR(1) parsing.
-- Here we show to to perform LL(n) parsing with it.
-------------------------------------------------------------------------------
procedure ASU_Example_4_46_Rd.Run is

   Test_File_Name : constant String := "Example.txt";

begin

   --------------------------------------------------------------------------
   -- Define the Grammar. The text in the example in the book looks something
   -- like:
   --
   -- S' -> S
   -- S  -> L = R | R
   -- L  -> * R | id
   -- R  -> L
   --

   S_Prime := S;
   S.all   := OpenToken.Token.Selection.Class(OpenToken.Token.Sequence.New_Instance(L & Equals & R & EOF) or
              OpenToken.Token.Sequence.New_Instance(R & EOF));
   -- Note the following line should probably work, but won't w/ gnat 3.12p or 3.13p. Try it on your
   -- system and see if it compiles:
   -- S.all   := new OpenToken.Token.Sequence.Instance'(L & Equals & R & EOF) or
   --            new OpenToken.Token.Sequence.Instance'(R & EOF);
   -- If it works, you can transform L's assignment this way as well.

   L.all   := OpenToken.Token.Selection.Class(OpenToken.Token.Sequence.New_Instance(Asterix & R) or ID);
   R.all   := L.all;


   Ada.Text_IO.Put ("Parsing file " & Test_File_Name & "...");
   Ada.Text_IO.Flush;

   Ada.Text_IO.Open (File => Input_File,
                     Name => Test_File_Name,
                     Mode => Ada.Text_IO.In_File
                     );

   -- Load up the first token
   Tokenizer.Find_Next (Analyzer);

   OpenToken.Token.Parse
     (Match    => S_Prime.all,
      Analyzer => Analyzer
      );

   Ada.Text_IO.Put_Line ("passed");
exception
   when Error : OpenToken.Token.Parse_Error =>
      Ada.Text_IO.Put_Line ("failed at line" & Integer'Image(Tokenizer.Line (Analyzer)) &
                            ", column" & Integer'Image(Tokenizer.Column (Analyzer)) &
                            " due to parse exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
end ASU_Example_4_46_Rd.Run;



