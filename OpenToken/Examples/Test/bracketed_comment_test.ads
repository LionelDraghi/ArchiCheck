-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein, 2000 Ted Dennison
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
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
-- $Log: bracketed_comment_test.ads,v $
-- Revision 1.1  2000/08/12 21:26:48  Ted
-- initial version
--
-- Revision 1.1  2000/02/05 04:04:34  Ted
-- Test driver for bracketed comment support.
--
-------------------------------------------------------------------------------
with Opentoken.Token.Enumerated.Analyzer;
with Opentoken.Recognizer.Bracketed_Comment;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.End_Of_File;

package Bracketed_Comment_Test is
   type Test_Token is
     (EmbeddedComment_T,  -- /* anything (even several lines) */
      Whitespace_T,
      End_of_File_T);

   package Master_Token is new Opentoken.Token.Enumerated (Test_Token);
   package Tokenizer is new Master_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (EmbeddedComment_T  => Tokenizer.Get (Opentoken.Recognizer.Bracketed_Comment.Get
                                           ("/*", "*.*..", Reportable => True)),
      Whitespace_T       => Tokenizer.Get (Opentoken.Recognizer.Character_Set.Get
                                           (Opentoken.Recognizer.Character_Set.Standard_Whitespace)),
      End_of_File_T      => Tokenizer.Get (Opentoken.Recognizer.End_Of_File.Get));

end Bracketed_Comment_Test;
