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


-------------------------------------------------------------------------------
--  This example is an implementation of Example 4.46 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). The example was meant to demonstrate basic LALR(1) parsing.
--  Here we show to to perform LL(n) parsing with it.
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Token.Selection;
with OpenToken.Token.Sequence;
procedure ASU_Example_4_46_RD.Run is

   use OpenToken.Token.Selection;
   use OpenToken.Token.Sequence;

   Test_File_Name : constant String := "Example.txt";

begin

   --------------------------------------------------------------------------
   --  Define the Grammar. The text in the example in the book looks something
   --  like:
   --
   --  S' -> S
   --  S  -> L = R | R
   --  L  -> * R | id
   --  R  -> L
   --

   S_Prime := S;
   S.all   := OpenToken.Token.Selection.Class
     (OpenToken.Token.Sequence.New_Instance (L & Equals & R & EOF) or
        OpenToken.Token.Sequence.New_Instance (R & EOF));

   --  Note the following would produce an accessiblity error;
   --  'new foo' is an anonymous access type declared in a procedure,
   --  so it can't be stored in a global access type.
   --  S.all   := new OpenToken.Token.Sequence.Instance'(L & Equals & R & EOF) or
   --            new OpenToken.Token.Sequence.Instance'(R & EOF);

   L.all   := OpenToken.Token.Selection.Class (OpenToken.Token.Sequence.New_Instance (Asterix & R) or ID);
   R.all   := L.all;

   Ada.Text_IO.Put ("Parsing file " & Test_File_Name & "...");
   Ada.Text_IO.Flush;

   Ada.Text_IO.Open (File => Input_File,
                     Name => Test_File_Name,
                     Mode => Ada.Text_IO.In_File);

   --  Load up the first token
   Tokenizer.Find_Next (Analyzer);

   OpenToken.Token.Parse
     (Match    => S_Prime.all,
      Analyzer => Analyzer
      );

   Ada.Text_IO.Put_Line ("passed");
exception
   when Error : OpenToken.Parse_Error =>
      Ada.Text_IO.Put_Line ("failed at line" & Integer'Image (Tokenizer.Line (Analyzer)) &
                            ", column" & Integer'Image (Tokenizer.Column (Analyzer)) &
                            " due to parse exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
end ASU_Example_4_46_RD.Run;



