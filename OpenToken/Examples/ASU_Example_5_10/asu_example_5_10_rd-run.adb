-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000 Ted Dennison
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
-- $Log: asu_example_5_10_rd-run.adb,v $
-- Revision 1.1  2000/08/12 21:33:06  Ted
-- Initial version
--
-- Revision 1.2  2000/02/05 04:02:21  Ted
-- Fixed integers to not use signs
--
-- Revision 1.1  2000/01/27 21:02:55  Ted
-- An implementation of example 5.10 from the dragon book. (a simple calculator)
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;

-------------------------------------------------------------------------------
-- This example is an implementation of Example 5.10 from "Compilers
-- Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
-- Dragon Book"). It demonstrates handling of synthesized attributes
-------------------------------------------------------------------------------
procedure ASU_Example_5_10_RD.Run is

   -- Create a user-settable text feeder, and a string buffer to fill it with
   Line        : String (1..1024);
   Line_Length : Natural;

   Parenthesized_Expression : aliased Expression_Sequence := Left_Paren & E & Right_Paren;
begin
   --------------------------------------------------------------------------
   -- Define the non-terminals. The text in the example in the book looks
   -- something like:
   --
   -- L -> E         print (L.val)
   -- E -> E + T     E.val := E1.val + T.val
   -- E -> T
   -- T -> T * F     T.val := T1.val * F.val
   -- T -> F
   -- F -> ( E )     F.val := E.val
   -- F -> digit
   --
   -- For Recursive-decent (LL) parsing we can't have recursive references
   -- in the first token in a token's definition. That would cause infinite
   -- recursion! This is the case in the definitions above for E and T.
   -- In most cases you can fix this by rearranging the grammar a bit so that
   -- the first token isn't a self-reference. But in this particular case
   -- one of the canned tokens, the List token, will do the job.
   L.all := E & EOF;
   E.all := Operation_List.Get (T, Plus);
   T.all := Operation_List.Get (F, Times);
   F.all := Int_Literal or Parenthesized_Expression'Unchecked_Access;
   -- Note the following line should probably work, but won't w/ gnat 3.12p. Try it on your system and see if
   -- it compiles:
   --   F.all := Int_Literal or new Expression_Sequence'(Left_Paren & E & Right_Paren);

   Ada.Text_IO.Put_Line ("A simple calculator, as specified in example 5.10 in Aho, Sethi, and Ullman's");
   Ada.Text_IO.Put_Line ("""Compilers Principles, Techniques and Tools""");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("""+"", ""*"", and ""( num )"" are understood.");
   Ada.Text_IO.Put_Line ("(Enter a blank line to quit)");

   -- Read and parse lines from the console until an empty line is read.
   loop
      Ada.Text_IO.Get_Line(Line, Line_Length);

      exit when Line_Length = 0;
      OpenToken.Text_Feeder.String.Set
        (Feeder => Feeder,
         Value  => Line (1..Line_Length));

      -- Load up the first token
      Tokenizer.Find_Next (Analyzer);

      OpenToken.Token.Parse
        (Match    => OpenToken.Token.Class(L.all),
         Analyzer => Analyzer
         );

      Ada.Text_IO.Put_Line (Integer'Image(L.Value));
   end loop;

end ASU_Example_5_10_RD.Run;
