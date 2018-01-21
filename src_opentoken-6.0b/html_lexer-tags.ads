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

with OpenToken.Text_Feeder;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Nothing;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.String;

pragma Elaborate_All (OpenToken.Recognizer.Character_Set, OpenToken.Recognizer.End_Of_File, OpenToken.Recognizer.Keyword,
                      OpenToken.Recognizer.Nothing, OpenToken.Recognizer.Separator, OpenToken.Recognizer.String);

private package HTML_Lexer.Tags is

   ---------------------------------------------------------------------
   -- This ia a lexical analyser for the HTML language. It analyses
   -- only the tag contents:
   --
   --    <Tag Attribute=Value Attribute="String"> not considered </Tag>
   --
   -- This very first version is not complete. It simply serves as a
   -- demonstration of feasibility.
   -- Missing tags and attributes are returned as Value.
   ---------------------------------------------------------------------

   type Tag_Token is (-- Tag delimiters
                      Start_Tag_Opener,  -- <
                      End_Tag_Opener,    -- </
                      Tag_Closer,        -- >

                      -- Tags (without delimiters, ... standing for attributes)
                      HTML,              -- <HTML ...>
                      Head,              -- <HEAD ...>
                      Meta,              -- <META ...>
                      HTML_Body,         -- <BODY ...>
                      Anchor,            -- <A ...>
                      Heading_1,         -- <H1 ...>
                      Image,             -- <IMG ...>

                      -- add further tags here
                      -- Attributes (the left side of assignments without = and following value)
                      Content,           -- CONTENT=
                      Hyper_Reference,   -- HREF=
                      Link_Type,         -- TYPE=
                      Name,              -- NAME=
                      Title,             -- TITLE=

                      -- add further attributes here
                      -- The assignment character in attributes
                      Assignment,        -- =

                      -- Values (the right side of assignments)
                      Value,             -- unquoted
                      String,            -- "quoted"

                      -- Syntax error
                      Bad_Token,
                      --
                      Whitespace, End_Of_Tag);

   package Master_Tag_Token is new OpenToken.Token.Enumerated
     (Tag_Token, Tag_Token'First, Tag_Token'Last, Tag_Token'Image);
   package Tokenizer is new Master_Tag_Token.Analyzer;

   use type Ada.Strings.Maps.Character_Set;

   Syntax: constant Tokenizer.Syntax :=
     (Start_Tag_Opener => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("<")),
      End_Tag_Opener   => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("</")),
      Tag_Closer       => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (">")),
      HTML             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("HTML")),
      Head             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("Head")),
      Meta             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("Meta")),
      HTML_Body        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("HTML_Body")),
      Heading_1        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("H1")),
      Anchor           => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("A")),
      Image            => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("IMG")),
      Content          => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CONTENT")),
      Hyper_Reference  => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("HREF")),
      Link_Type        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TYPE")),
      Name             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NAME")),
      Title            => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TITLE")),
      Assignment       => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("=")),
      Value            => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (Ada.Strings.Maps.Constants.Letter_Set        or
              Ada.Strings.Maps.Constants.Decimal_Digit_Set or
              Ada.Strings.Maps.To_Set (".-"),
            Reportable => True)),
      String           => Tokenizer.Get(OpenToken.Recognizer.String.Get (Double_Delimiter => False)),
      Whitespace       => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Bad_Token        => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get),
      End_Of_Tag       => Tokenizer.Get(OpenToken.Recognizer.End_Of_File.Get));

   Tag_Input_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   Analyzer: Tokenizer.Instance := Tokenizer.Initialize
     (Syntax,
      Feeder  => Tag_Input_Feeder'access).all;

end HTML_Lexer.Tags;
