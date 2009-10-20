--  Abstract :
--
--  Test OpenToken.Recognizer.Based_Integer
--
--  Copyright (C) 2006 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with OpenToken.Recognizer.Based_Integer; use OpenToken.Recognizer.Based_Integer;
procedure Recognizer_Based_Integer_Test
is
   Recognizer : Instance := Get;

   procedure Test
     (Item         : in String;
      Expect_Match : in Boolean;
      Comment      : in String)
   is
      --  Analyze all of Item, compare final Verdict to Expect_Match.
      use OpenToken.Recognizer;
      Verdict : Analysis_Verdict;
      Last    : Integer := Item'First;
   begin
      Clear (Recognizer);

      loop
         Analyze (Recognizer, Item (Last), Verdict);
         exit when Last = Item'Last or Verdict = Failed;
         Last := Last + 1;
      end loop;

      if Expect_Match then
         if Verdict = Failed then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            Put_Line (Comment & "...failed");
         else
            Put_Line (Comment & "...passed");
         end if;
      else
         --  not Expect_Match
         if Verdict = Failed then
            Put_Line (Comment & "...passed");
         else
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            Put_Line (Comment & "...failed");
         end if;
      end if;
   exception
   when E : others =>
      Put_Line
        (Comment & "...failed due to exception. " &
           Ada.Exceptions.Exception_Name (E) &
           ": " & Ada.Exceptions.Exception_Message (E));
   end Test;

   procedure Test_Syntax
     (Item         : in String;
      Comment      : in String;
      Expected     : in String)
   is
      --  Assume Item contains one more character after valid
      --  based_integer; compare matched string to Expected.
      use OpenToken.Recognizer;
      Verdict : Analysis_Verdict;
      Last    : Integer := Item'First;
   begin
      Clear (Recognizer);

      loop
         Analyze (Recognizer, Item (Last), Verdict);
         exit when Last = Item'Last or Verdict = Failed;
         Last := Last + 1;
      end loop;

      if Verdict = Matches then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         Put_Line (Comment & " ... failed (matched extra stuff)");
      else
         if Expected = Item (Item'First .. Last - 1) then
            Put_Line (Comment & "...passed");
         else
            Put_Line (Comment & "...failed; got '" & Item (Item'First .. Last - 1) & "'");
         end if;
      end if;
   exception
   when E : others =>
      Put_Line
        (Comment & "...failed due to exception. " &
           Ada.Exceptions.Exception_Name (E) &
           ": " & Ada.Exceptions.Exception_Message (E));
   end Test_Syntax;

begin
   Test
     (Item         => "1234",
      Expect_Match => True,
      Comment      => "simple integer");

   Test_Syntax
     (Item         => "1234;",
      Expected     => "1234",
      Comment      => "trailing syntax");

   Test
     (Item         => "-1234",
      Expect_Match => True,
      Comment      => "negative integer");

   Test_Syntax
     (Item         => "10#1234#;",
      Expected     => "10#1234#",
      Comment      => "base 10");

   Test
     (Item         => "1234#;",
      Expect_Match => False,
      Comment      => "trailing #");

   Test
     (Item         => "10#-1234#",
      Expect_Match => False,
      Comment      => "bad negative");

   Test_Syntax
     (Item         => "-10#1234#)",
      Expected     => "-10#1234#",
      Comment      => "base 10, negative");

   Test
     (Item         => "16#12AF#",
      Expect_Match => True,
      Comment      => "base 16");

   Test
     (Item         => "16_#12AF#",
      Expect_Match => False,
      Comment      => "bad base 16");

   Test
     (Item         => "2#1200#",
      Expect_Match => False,
      Comment      => "base 2, fail");

   Test
     (Item         => "2#100#",
      Expect_Match => True,
      Comment      => "base 2, pass");

   Test
     (Item         => "     -- A comment :",
      Expect_Match => False,
      Comment      => "Ada comment");

end Recognizer_Based_Integer_Test;
