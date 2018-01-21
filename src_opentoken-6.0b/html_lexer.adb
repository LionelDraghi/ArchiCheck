-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2010 Stephen Leake
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

with Ada.Characters.Latin_1;
with Ada.Strings.Maps.Constants;
with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.HTML_Entity;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Nothing;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.String;
package body HTML_Lexer is

   function Name (Token : HTML_Token) return Token_Name is
   begin
      return Token.Name;
   end Name;

   function Lexeme (Token : HTML_Token) return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String (Token.Lexeme);
   end Lexeme;

   function Line (Token : in HTML_Token) return Natural
   is begin
      return Token.Line;
   end Line;

   function Column (Token : in HTML_Token) return Natural
   is begin
      return Token.Column;
   end Column;

   use type Ada.Strings.Maps.Character_Set;

   HTML_Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
     (Ada.Characters.Latin_1.HT &
        Ada.Characters.Latin_1.CR &
        Ada.Characters.Latin_1.LF &
        Ada.Characters.Latin_1.Space);

   function Text_Syntax return Tokenizer.Syntax
   is begin
      return
        (Document_Type        => Tokenizer.Get
           (OpenToken.Recognizer.Bracketed_Comment.Get
              (Comment_Opener => "<!",
               Comment_Closer => ">",
               Reportable     => True)),
         Start_Tag_Opener     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<")),
         End_Tag_Opener       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("</")),
         Text                 => Tokenizer.Get
           (OpenToken.Recognizer.Character_Set.Get
              (Ada.Strings.Maps.Constants.Graphic_Set - Ada.Strings.Maps.To_Set ("<&"),
               Reportable     => True)),
         Entity               => Tokenizer.Get (OpenToken.Recognizer.HTML_Entity.Get),

         --  See HTML definition section 3.2.4 Comments; HTML syntax
         --  actually allows whitespace in the comment closer: "-- >"
         --  is a closer. That also means that "<! foo -- bar>" is
         --  invalid syntax. But we don't have a recognizer that can
         --  deal with that, and this is good enough for common usage.
         Comment              => Tokenizer.Get
           (OpenToken.Recognizer.Bracketed_Comment.Get
              (Comment_Opener => "<!--",
               Comment_Closer => "-->",
               Reportable     => True)),
         Whitespace           => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (HTML_Whitespace)),
         Bad_Token            => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
         End_Of_File          => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
         Pre                  => Tokenizer.Get
           (OpenToken.Recognizer.Bracketed_Comment.Get
              (Comment_Opener => "<pre>",
               Comment_Closer => "</pre>",
               Reportable     => True)),
         others               => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get));
   end Text_Syntax;

   function Tag_Syntax return Tokenizer.Syntax
   is begin
      return
        (Tag_Closer       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">")),
         HTML             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("HTML")),
         Head             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("Head")),
         Meta             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("Meta")),
         HTML_Body        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("Body")),
         Heading_1        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("H1")),
         Anchor           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("A")),
         Image            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("IMG")),
         Content          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("CONTENT")),
         Hyper_Reference  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("HREF")),
         Link_Type        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TYPE")),
         Name             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("NAME")),
         Source           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("SRC")),
         Title            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("TITLE")),
         Assignment       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=")),
         Value            => Tokenizer.Get
           (OpenToken.Recognizer.Character_Set.Get
              (Ada.Strings.Maps.Constants.Letter_Set        or
                 Ada.Strings.Maps.Constants.Decimal_Digit_Set or
                 Ada.Strings.Maps.To_Set (".-_/:#?',*%"), --  unquoted URIs, framesets
               Reportable => True)),
         String           => Tokenizer.Get (OpenToken.Recognizer.String.Get (Double_Delimiter => False)),
         Whitespace       => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (HTML_Whitespace)),
         Bad_Token        => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
         End_Of_File      => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
         others           => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get));
   end Tag_Syntax;

end HTML_Lexer;
