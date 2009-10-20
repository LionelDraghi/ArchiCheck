-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephen Leake
-- Copyright (C) 1999, 2000 Christoph Karl Walter Grein
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

with Ada.Strings.Unbounded;
with OpenToken.Token.Enumerated.Analyzer;
package HTML_Lexer is

   ----------------------------------------------------------------------
   --  Utilities for a lexical analyser for the HTML language.
   --
   --  See the child packages HTML_Lexer.Task_Safe, .Task_Unsafe for
   --  the actual lexers.
   --
   ----------------------------------------------------------------------

   type Token_Name is
     (
      --  Syntax error
      Bad_Token,

      --  Comments <!-- anything -->
      Comment,
      Whitespace,

      --  Document Type Declaration <!DOCTYPE attributes>
      Document_Type,

      --  Tag delimiters
      Start_Tag_Opener,  -- <
      End_Tag_Opener,    -- </
      Tag_Closer,        -- >

      --  Tags (without delimiters), not all tags may have attributes
      HTML,              -- <HTML attributes>
      Head,              -- <HEAD attributes>
      Title,             -- <TITLE attributes>
      Meta,              -- <META attributes>
      HTML_Body,         -- <BODY attributes>
      Heading_1,         -- <H1 attributes>
      Anchor,            -- <A attributes>
      Image,             -- <IMG attributes>

      --  add further tags here
      --  Attributes (Attribute=value)
      Content,           -- CONTENT
      Hyper_Reference,   -- HREF
      Name,              -- NAME
      Link_Type,         -- TYPE
      Source,            -- SRC

      --  add further attributes here
      --  The assignment character in attributes
      Assignment,        -- =

      --  Values (the right side of assignments)
      Value,             -- unquoted
      String,            -- "quoted"

      --  Running text and entities like &amp;
      Text,
      Entity,

      End_Of_File);

   type HTML_Token is private;

   function Name   (Token : in HTML_Token) return Token_Name;
   function Lexeme (Token : in HTML_Token) return Standard.String;
   function Line   (Token : in HTML_Token) return Natural;
   function Column (Token : in HTML_Token) return Natural;
private

   type HTML_Token is record
      Name   : Token_Name;
      Lexeme : Ada.Strings.Unbounded.Unbounded_String;
      Line   : Natural;
      Column : Natural;
   end record;

   --  Visible for children
   package Master_Token is new OpenToken.Token.Enumerated (Token_Name);
   package Tokenizer is new Master_Token.Analyzer;

   -----------------------------------------------------------------------
   --  HTML syntax is very different from Ada or Java syntax. This is an
   --  abbreviated excerpt of the HTML 4.0 Reference.
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
   --  Thus the text between tokens is arbitrary and need not be analysed.
   --  In fact the whole text between tags is treated as a token of its
   --  own (entities excepted).
   --  Inside tags, however, we want to analyse for tag names, attribute
   --  names and attribute values.
   --  Thus we have to analyse the HTML document after an opening "<" and
   --  stop after a closing ">".
   --
   --  So the idea is the following:
   --  We split the syntax into two parts: A text and a tag syntax.
   --  The lexer starts with the text syntax, and every time a tag opener
   --  or closer is hit, we change the syntax to the appropriate one.
   --
   --  When defining the syntaxes, the following has to be taken into
   --  account:
   --  Since the syntax has to contain all token names, unused (and hence
   --  in this syntax illegal) names use the Nothing recognizer. In order
   --  to return them as Bad_Token, this name has to come first in the
   --  sequence of names.
   --  Since Document_Type and Comment both use the Bracketed_Comment
   --  recognizer with the same opening string "<!", Comment has to come
   --  first in the sequence of names.
   --
   --  If Document_Type is to be analyzed further like other tags, the
   --  same trick with switching syntaxes can be applied.
   -----------------------------------------------------------------------

   function Text_Syntax return Tokenizer.Syntax;
   function Tag_Syntax return Tokenizer.Syntax;
   --  These must be functions, not constants, because they contain
   --  pointers, and we don't have a deep copy defined.

end HTML_Lexer;
