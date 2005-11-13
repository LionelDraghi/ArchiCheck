-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2000 Christoph Karl Walter Grein
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
-- $Log: html_lexer.adb,v $
-- Revision 1.3  2000/08/07 00:22:54  Ted
-- Change to work w/ new package hierarchy
--
-- Revision 1.2  2000/01/27 21:21:39  Ted
-- Fix to work with new text feeder routines
--
-- Revision 1.1  1999/12/27 21:41:57  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/22  Grein
-- Initial Version
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps.Constants;

with OpenToken.Token.Enumerated.Analyzer;

with Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.String;
with Opentoken.Recognizer.Character_Set, Opentoken.Recognizer.HTML_Entity;
with Opentoken.Recognizer.Bracketed_Comment;
with Opentoken.Recognizer.Nothing;
with Opentoken.Recognizer.End_Of_File;

pragma Elaborate_All (Opentoken.Token.Enumerated, Opentoken.Token.Enumerated.Analyzer,
                      Opentoken.Recognizer.Keyword, Opentoken.Recognizer.Separator,
                      Opentoken.Recognizer.String,
                      Opentoken.Recognizer.Character_Set, Opentoken.Recognizer.HTML_Entity,
                      Opentoken.Recognizer.Bracketed_Comment,
                      Opentoken.Recognizer.Nothing,
                      Opentoken.Recognizer.End_Of_File);

package body HTML_Lexer is

  -----------------------------------------------------------------------
  -- This package has been implemented as a test of feasibility.
  -- The implementation is open to improvements.
  -----------------------------------------------------------------------
  -- HTML syntax is very different from Ada or Java syntax. This is an
  -- abbreviated excerpt of the HTML 4.0 Reference.
  --
  --    Elements are the structures that describe parts of an HTML
  --    document ... An element has three parts: a start tag, content,
  --    and an end tag. A tag is special text--"markup"--that is
  --    delimited by "<" and ">". An end tag includes a "/" after the
  --    "<" ... The start and end tags surround the content of the
  ---   element:
  --       <EM>This is emphasized text</EM>
  --    ... An element's attributes define various properties for the
  --    element ...
  --
  --       <IMG SRC="wdglogo.gif" ALT="Web Design Group">
  --
  --    An attribute is included in the start tag only--never the end
  --    tag--and takes the form Attribute-name="Attribute-value".
  --
  -- Thus the text between tokens is arbitrary and need not be analysed.
  -- In fact the whole text between tags is treated as a token of its
  -- own (entities excepted).
  -- Inside tags, however, we want to analyse for tag names, attribute
  -- names and attribute values.
  -- Thus we have to analyse the HTML document after an opening "<" and
  -- stop after a closing ">".
  --
  -- So the idea is the following:
  -- We split the syntax into two parts: A text and a tag syntax.
  -- The lexer starts with the text syntax, and every time a tag opener
  -- or closer is hit, we change the syntax to the appropriate one.
  --
  -- When defining the syntaxes, the following has to be taken into
  -- account:
  -- Since the syntax has to contain all token names, unused (and hence
  -- in this syntax illegal) names use the Nothing recognizer. In order
  -- to return them as Bad_Token, this name has to come first in the
  -- sequence of names.
  -- Since Document_Type and Comment both use the Bracketed_Comment
  -- recognizer with the same opening string "<!", Comment has to come
  -- first in the sequence of names.
  --
  -- If Document_Type is to be analyzed further like other tags, the
  -- same trick with switching syntaxes can be applied.
  -----------------------------------------------------------------------

  use type Ada.Strings.Maps.Character_Set;

  package Master_Token is new OpenToken.Token.Enumerated (Token_Name);
  package Tokenizer is new Master_Token.Analyzer;

  Text_Syntax: constant Tokenizer.Syntax :=
    (Document_Type    => Tokenizer.Get(OpenToken.Recognizer.Bracketed_Comment.Get
                                          (Comment_Opener => "<!",
                                           Comment_Closer => ">",
                                           Reportable     => True)),
     Start_Tag_Opener => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("<")),
     End_Tag_Opener   => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("</")),
     Text             => Tokenizer.Get(OpenToken.Recognizer.Character_Set.Get
                                          (Ada.Strings.Maps.Constants.Graphic_Set -
                                           Ada.Strings.Maps.To_Set ("<>""&"),
                                           Reportable => True)),
     Entity           => Tokenizer.Get(OpenToken.Recognizer.HTML_Entity.Get),
     Comment          => Tokenizer.Get(OpenToken.Recognizer.Bracketed_Comment.Get
                                          (Comment_Opener => "<!--",
                                           Comment_Closer => "-->",
                                           Reportable => True)),
     Whitespace       => Tokenizer.Get(OpenToken.Recognizer.Character_Set.Get
                                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
     Bad_Token        => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get),
     End_Of_File      => Tokenizer.Get(OpenToken.Recognizer.End_Of_File.Get),
     others           => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get));

  Tag_Syntax: constant Tokenizer.Syntax :=
    (Tag_Closer       => Tokenizer.Get(OpenToken.Recognizer.Separator.Get (">")),
     HTML             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("HTML")),
     Head             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("Head")),
     Meta             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("Meta")),
     HTML_Body        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("Body")),
     Heading_1        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("H1")),
     Anchor           => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("A")),
     Image            => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("IMG")),
     Content          => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("CONTENT")),
     Hyper_Reference  => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("HREF")),
     Link_Type        => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TYPE")),
     Name             => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("NAME")),
     Title            => Tokenizer.Get(OpenToken.Recognizer.Keyword.Get ("TITLE")),
     Assignment       => Tokenizer.Get(OpenToken.Recognizer.Separator.Get ("=")),
     Value            => Tokenizer.Get(OpenToken.Recognizer.Character_Set.Get
                                          (Ada.Strings.Maps.Constants.Letter_Set        or
                                           Ada.Strings.Maps.Constants.Decimal_Digit_Set or
                                           Ada.Strings.Maps.To_Set (".-"),
                                           Reportable => True)),
     String           => Tokenizer.Get(OpenToken.Recognizer.String.Get (Double_Delimiter => False)),
     Whitespace       => Tokenizer.Get(OpenToken.Recognizer.Character_Set.Get
                                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
     Bad_Token        => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get),
     End_Of_File      => Tokenizer.Get(OpenToken.Recognizer.End_Of_File.Get),
     others           => Tokenizer.Get(OpenToken.Recognizer.Nothing.Get));

  Analyzer: Tokenizer.Instance := Tokenizer.Initialize (Text_Syntax, Default => Bad_Token);

  procedure Initialize (Input_Feeder: in OpenToken.Text_Feeder.Text_IO.Instance) is
  begin
    Tokenizer.Input_Feeder := Input_Feeder;
  end Initialize;

  function Name (Token: HTML_Token) return Token_Name is
  begin
    return Token.Name;
  end Name;

  function Lexeme (Token: HTML_Token) return Standard.String is
  begin
    return Ada.Strings.Unbounded.To_String (Token.Lexeme);
  end Lexeme;

  function Next_Token return HTML_Token is
    Result: HTML_Token;
  begin
    Tokenizer.Find_Next (Analyzer);
    Result := (Name   => Tokenizer.Id (Analyzer),
               Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                              (Tokenizer.Lexeme (Analyzer)));
    case Result.Name is
      when Start_Tag_Opener | End_Tag_Opener =>
        Tokenizer.Set_Syntax (Analyzer, Tag_Syntax);
      when Tag_Closer =>
        Tokenizer.Set_Syntax (Analyzer, Text_Syntax);
      when others =>
        null;
    end case;
    return Result;
  end Next_Token;

end HTML_Lexer;
