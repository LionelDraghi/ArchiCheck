-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Opentoken;


procedure Archicheck.Rules_Parser.Parse is
   Test_File_Name : constant String := "./rules.txt";

begin
   OpenToken.Trace_Parse := 1; -- debug level

   Put_Line ("------------------------- Parsing file " & Test_File_Name & "...");
   Flush;

   Archicheck.Rules_Parser.Parse (Test_File_Name);

   Ada.Text_IO.Put_Line ("------------------------- passed");

end Archicheck.Rules_Parser.Parse;
