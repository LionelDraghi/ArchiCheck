-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2013, 2015 Christoph Karl Walter Grein
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
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
-------------------------------------------------------------------------------

with Ada.Strings.Maps.Constants;

with OpenToken.Token.Enumerated.Analyzer;

with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.HTML_Entity;
with OpenToken.Recognizer.Nothing;
private package HTML_Lexer.Basic is

   type Basic_Token is (Whitespace,
                        -- Comments <!-- anything -->
                        Comment,
                        -- Document Type Declaration <!DOCTYPE ... >
                        Doctype,
                        -- HTML tags like <A> or </A>
                        HTML_Tag,
                        -- Running text and entities like &amp;
                        Text, Entity,
                        -- Syntax error
                        Bad_Token,
                        --
                        End_Of_File);
   -- Note that sequence of tokens is relevant since among competing
   -- tokens the first one wins.

   package Master_Basic_Token is new OpenToken.Token.Enumerated
     (Basic_Token, Basic_Token'First, Basic_Token'Last, Basic_Token'Image);
   package Tokenizer is new Master_Basic_Token.Analyzer;

   use type Ada.Strings.Maps.Character_Set;

   Syntax: constant Tokenizer.Syntax :=
     (Doctype     => Tokenizer.Get
        (OpenToken.Recognizer.Bracketed_Comment.Get
           (Comment_Opener => "<!",
            Comment_Closer => ">",
            Reportable     => True)),
      HTML_Tag    => Tokenizer.Get
        (OpenToken.Recognizer.Bracketed_Comment.Get
           (Comment_Opener => "<",
            Comment_Closer => ">",
            Reportable     => True)),
      Text        => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (Ada.Strings.Maps.Constants.Graphic_Set - Ada.Strings.Maps.To_Set ("<>""&"),
            Reportable => True)),
      Entity      => Tokenizer.Get(OpenToken.Recognizer.HTML_Entity.Get),
      Comment     => Tokenizer.Get
        (OpenToken.Recognizer.Bracketed_Comment.Get
           (Comment_Opener => "<!--",
            Comment_Closer => "-->",
            Reportable => True)),
      Whitespace  => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Bad_Token   => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get),
      End_Of_File => Tokenizer.Get(OpenToken.Recognizer.End_Of_File.Get));

   Analyzer: Tokenizer.Instance := Tokenizer.Initialize (Syntax).all;

end HTML_Lexer.Basic;
