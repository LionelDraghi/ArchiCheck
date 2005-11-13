-------------------------------------------------------------------------------
--
-- Copyright (C) 1999,2000 FlightSafety International and Ted Dennison
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
-- This software was originally developed by the following company, and was
-- released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-- Update History:
-- $Log: asu_example_3_6-run.adb,v $
-- Revision 1.1  2000/08/12 15:15:20  Ted
-- moved from asu_example_3_6
--
-- Revision 1.5  2000/01/27 21:10:09  Ted
-- Fixed to work with 2.0
--
-- Revision 1.4  1999/12/27 19:56:04  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.3  1999/10/08 23:17:26  Ted
-- Fix minor transposition bug
--
-- Revision 1.2  1999/08/17 03:27:13  Ted
-- Add log line
--
-------------------------------------------------------------------------------
with Ada.Text_IO;

with OpenToken.Text_Feeder.Text_IO;
with Relop_Example_Token;

---------------------------------------------------------------------------
-- This example is an implementation of Example 3.6 from "Compilers
-- Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
-- Dragon Book").
---------------------------------------------------------------------------
procedure Asu_Example_3_6.Run is

   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;

   File_Name : constant String := "asu.txt";

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);
begin

   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => File_Name
      );

   Ada.Text_IO.Set_Input (File);
   Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

   for Q in 1..10 loop
      Tokenizer.Find_Next (Analyzer);

      Ada.Text_IO.Put_Line ("Found " & Example_Token_ID'Image (Tokenizer.ID (Analyzer)));
   end loop;

end Asu_Example_3_6.Run;
