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
-- $Log: html_lexer.ads,v $
-- Revision 1.2  2000/08/06 23:37:55  Ted
-- Change to work w/ new package hierarchy
--
-- Revision 1.1  1999/12/27 21:41:57  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/22  Grein
-- Initial Version
--
-- Revision 0.0  1999/12/16  Grein
-- PDL
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with OpenToken.Text_Feeder.Text_IO;

pragma Elaborate_All (Opentoken.Text_Feeder.Text_IO);

package HTML_Lexer is

  ------------------------------------------------------------------------
  -- This ia a lexical analyser for the HTML language.
  --
  --   <Tag Attribute=Value Attribute="String"> Text with &entity; </Tag>
  --
  -- This very first version is not complete. It simply serves as a
  -- demonstration of feasibility.
  ------------------------------------------------------------------------

  type Token_Name is (-- Syntax error
                      Bad_Token,
                      -- Comments <!-- anything -->
                      Comment,
                      Whitespace,
                      -- Document Type Declaration <!DOCTYPE attributes>
                      Document_Type,
                      -- Tag delimiters
                      Start_Tag_Opener,  -- <
                      End_Tag_Opener,    -- </
                      Tag_Closer,        -- >
                      -- Tags (without delimiters), not all tags may have
                      -- attributes
                      HTML,              -- <HTML attributes>
                      Head,              -- <HEAD attributes>
                      Title,             -- <TITLE attributes>
                      Meta,              -- <META attributes>
                      HTML_Body,         -- <BODY attributes>
                      Heading_1,         -- <H1 attributes>
                      Anchor,            -- <A attributes>
                      Image,             -- <IMG attributes>
                         -- add further tags here
                      -- Attributes (Attribute=value)
                      Content,           -- CONTENT
                      Hyper_Reference,   -- HREF
                      Name,              -- NAME
                      Link_Type,         -- TYPE
                        -- add further attributes here
                      -- The assignment character in attributes
                      Assignment,        -- =
                      -- Values (the right side of assignments)
                      Value,             -- unquoted
                      String,            -- "quoted"
                      -- Running text and entities like &amp;
                      Text, Entity,
                      --
                      End_Of_File);

  procedure Initialize (Input_Feeder: in OpenToken.Text_Feeder.Text_IO.Instance);

  type HTML_Token is private;

  function Next_Token return HTML_Token;

  function Name   (Token: HTML_Token) return Token_Name;
  function Lexeme (Token: HTML_Token) return Standard.String;

private

  type HTML_Token is record
     Name  : Token_Name;
     Lexeme: Ada.Strings.Unbounded.Unbounded_String;
  end record;

end HTML_Lexer;
