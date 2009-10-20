--  Abstract :
--
--  Test OpenToken.Recognizer.Integer
--
--  Copyright (C) 2002, 2009 Stephen Leake.  All Rights Reserved.
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
with OpenToken.Recognizer.Integer; use OpenToken.Recognizer.Integer;
procedure Recognizer_Integer_Test
is
   Recognizer : Instance := Get;

   procedure Test
     (Item         : in String;
      Expect_Match : in Boolean;
      Comment      : in String)
   is
      use OpenToken.Recognizer;
      Verdict : Analysis_Verdict;
      I       : Integer := Item'First;
   begin
      Clear (Recognizer);

      loop
         Analyze (Recognizer, Item (I), Verdict);
         if (Expect_Match and Verdict = Failed) or
           ((not Expect_Match) and Verdict = Matches)
         then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            Put_Line (Comment & "...failed");
            return;
         end if;

         exit when I = Item'Last;
         I := I + 1;
      end loop;

      Put_Line (Comment & "...passed");
   exception
      when E : others =>
         Put_Line (Comment & "...failed due to exception. " &
                     Ada.Exceptions.Exception_Name (E) &
                     ": " & Ada.Exceptions.Exception_Message (E));
   end Test;

begin
   Test
     (Item         => "1234",
      Expect_Match => True,
      Comment      => "simple integer");

   Test
     (Item         => "-- A comment :)",
      Expect_Match => False,
      Comment      => "Ada comment");

   --  Tests for Debian bug 536359
   Test
     (Item         => "-1234",
      Expect_Match => True,
      Comment      => "negative integer");

   Test
     (Item         => "+1234",
      Expect_Match => True,
      Comment      => "explicitly positive integer");

end Recognizer_Integer_Test;
