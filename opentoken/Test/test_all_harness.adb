--  Abstract :
--
--  Run all OpenToken AUnit tests; see Makefile for other tests.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with OpenToken.Recognizer.CSV_Field.Test;
with Test_LR0_Kernels;
with Test_Statement_Actions;
with Test_Token_Identifier_Real_String;
procedure Test_All_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   Result : AUnit.Test_Results.Result;

begin
   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new OpenToken.Recognizer.CSV_Field.Test.Test_Case);
   Add_Test (Suite, new Test_LR0_Kernels.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Statement_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Token_Identifier_Real_String.Test_Case (Debug => False));

   --  end test cases

   Run (Suite.all, Result);

   --  Provide command line option -v to set verbose mode
   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_All_Harness;
