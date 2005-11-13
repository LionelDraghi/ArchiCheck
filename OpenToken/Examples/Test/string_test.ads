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
-- $Log: string_test.ads,v $
-- Revision 1.1  2000/08/12 21:15:07  Ted
-- initial version
--
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.End_Of_File;
with Opentoken.Recognizer.Keyword;
with Opentoken.Recognizer.String;
with OpenToken.Text_Feeder.Text_IO;

with Opentoken.Token.Enumerated;
with Opentoken.Token.Enumerated.Analyzer;

-------------------------------------------------------------------------------
-- This package provides the library-level declarations for running the string
-- test driver.
-------------------------------------------------------------------------------
package String_Test is


   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;

   File_Name : constant String := "String_Test.txt";

   type Example_Token_ID is (If_ID, String_ID, Whitespace, EOF);

   package Master_Example_Token is new Opentoken.Token.Enumerated (Example_Token_ID);
   package Tokenizer is new Master_Example_Token.Analyzer;

   Ada_Syntax : constant Tokenizer.Syntax :=
     (If_ID      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("if")),
      String_ID  => Tokenizer.Get(Opentoken.Recognizer.String.Get),
      Whitespace => Tokenizer.Get(Opentoken.Recognizer.Character_Set.Get
                                  (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF        => Tokenizer.Get(Opentoken.Recognizer.End_Of_File.Get)
      );

   C_Syntax : constant Tokenizer.Syntax :=
     (If_ID      => Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("if")),
      String_ID  => Tokenizer.Get(Opentoken.Recognizer.String.Get(Escapeable => True)),
      Whitespace => Tokenizer.Get(Opentoken.Recognizer.Character_Set.Get
                                  (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF        => Tokenizer.Get(Opentoken.Recognizer.End_Of_File.Get)
      );

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Ada_Syntax);


end String_Test;
