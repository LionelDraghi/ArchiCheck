-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000 Ted Dennison
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
with Ada.Text_IO;

-------------------------------------------------------------------------------
--  This example is an implementation of Example 4.46 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). It demonstrates basic LALR(1) parsing
-------------------------------------------------------------------------------
procedure ASU_Example_4_46.Run is

   --  The lalr parser instance.
   Test_Parser : LALR_Parser.Instance :=
     LALR_Parser.Generate (Grammar  => Grammar,
                           Analyzer => Analyzer
                           );

   Test_File_Name : constant String := "Example.txt";
begin

   Ada.Text_IO.Put ("Parsing file " & Test_File_Name & "...");
   Ada.Text_IO.Flush;

   Ada.Text_IO.Open (File => Input_File,
                     Name => Test_File_Name,
                     Mode => Ada.Text_IO.In_File
                     );

   --  Uncomment the following line to get a look at the parser
--   LALR_Parser.Print_Table (Test_Parser);

   LALR_Parser.Parse (Test_Parser);

   Ada.Text_IO.Put_Line ("passed");
end ASU_Example_4_46.Run;



