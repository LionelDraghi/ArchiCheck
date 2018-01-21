-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2012, 2013, 2014 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
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
--
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package is the top of a generic hierarchy. Based on the list
--  of IDs it is instantiated with, a user can create tokens and token
--  analyzers.
--
--  This package declares a type for designating a single token. It
--  is designed to be created by an instance of the Token.Analyzer
--  class when a particular kind of token is recognized.
--
--  Packages implementing a child of this type need to include a
--  constructor for the token analyzer and any nessecary utility
--  routines their parser may require.
-----------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with OpenToken.Recognizer;
with OpenToken.Text_Feeder;
generic

   type Token_ID is (<>);

   --  Tokens in the range Token_ID'First .. Pred (First_Terminal) are
   --  non-reporting (comments, whitespace), and thus are not used in
   --  generating an LALR grammar.
   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;
   --  Tokens in the range Succ (Last_Terminal) .. Token_ID'Last are
   --  the nonterminals of a grammar.

   with function Token_Image (Item : in Token_ID) return String;

package OpenToken.Token.Enumerated is

   subtype Terminal_ID is Token_ID range First_Terminal .. Last_Terminal;
   --  We can't define Nonterminal_ID here, because if Last_Terminal = Token_ID'last, there are no nonterminals.

   type Instance is new OpenToken.Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding
   function Image (Token : in Instance) return String;

   procedure Free (Item : in out Handle);

   type Recognizer_Handle is access all OpenToken.Recognizer.Class;
   --  Defined here rather than in Opentoken.Recognizer to allow
   --  access's of objects declared at the same level as this
   --  package's instantiation.

   type Action is access procedure (Token : in out Instance'Class);

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class;
   --  Get a token with ID, Name, and Build. Build will be called by
   --  Parse. Result is class-wide so derived types don't have to
   --  override Get.

   function "+" (Item : in Token_ID) return Instance'Class;
   --  Calls Get with default Name, Build; for use in LALR Grammar statements.

   procedure Set_Build (Token : in out Instance'Class; Build : in Action);

   type Buffer_Range is record
      Begin_Pos : Integer;
      End_Pos   : Integer;
   end record;

   Null_Buffer_Range : constant Buffer_Range := (Integer'Last, Integer'First);

   ----------------------------------------------------------------------
   --  Create will be called from Find_Next when a token is
   --  recognized.
   --
   --  Lexeme is the matched input text.
   --
   --  Bounds is the start and end position of the input text,
   --  relative to the last Analyzer Reset.
   --
   --  Recognizer is the recognizer that matched the token.
   --
   --  New_Token is the token that the analyzer associates with
   --  Recognizer (specified when the syntax is created).
   --
   --  Create is called even when Look_Ahead is true (when the parse
   --  is inactive), so that Lexeme and Recognizer can be preserved in
   --  the lookahead queue if needed; New_Token is pushed on the
   --  lookahead queue if another token is read with Look_Ahead True.
   --
   --  The recognizer is useful in creating tightly coupled pairs of
   --  tokens and recognizers. This allows communication of
   --  user-defined information global to the analyzer instance while
   --  maintaining overall re-entrancy.
   ----------------------------------------------------------------------
   procedure Create
     (Lexeme     : in     String;
      Bounds     : in     Buffer_Range;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance)
     is null;

   --------------------------------------------------------------------
   --  Copy From to To. Called by Enumerated.Parse when a token
   --  matches, when Actively is true. This is just a dispatching
   --  version of ':='; see the comments in Parse for more rationale.
   --
   --  Parse has verified that From'Tag = To'Tag, and that From.ID =
   --  To.ID.
   --------------------------------------------------------------------
   procedure Copy
     (To   : in out Instance;
      From : in     OpenToken.Token.Class)
     is null;

   --  Return a newly allocated copy of Token, or null
   function Copy (Token : in Handle) return Handle;

   --------------------------------------------------------------------------
   --  This function returns the ID of the token. This is made
   --  class-wide so it won't be overridable. That is done because
   --  some child packages access the ID directly, so overriding this
   --  routine would lead to inconsistent results.
   --------------------------------------------------------------------------
   function ID (Token : in Instance'Class) return Token_ID;

   ----------------------------------------------------------------------------
   --  Set the given token's ID to the given value
   ----------------------------------------------------------------------------
   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID);

   --------------------------------------------------------------------
   --  If Match matches the current token, and Actively is True,
   --  copies the results of the earlier call to Create to Match, and
   --  calls Build (if not null).
   --------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True);

   overriding function Name (Token : in Instance) return String;

   type Source is abstract new OpenToken.Token.Source with record
      Feeder : OpenToken.Text_Feeder.Text_Feeder_Ptr;
   end record;

   --------------------------------------------------------------------------
   --  Returns the actual text of the last token that was matched.
   --
   --  Raises Programmer_Error when the last token was read from the
   --  lookahead queue.
   --------------------------------------------------------------------------
   function Lexeme (Analyzer : in Source) return String is abstract;

   --------------------------------------------------------------------------
   --  Returns the recognizer handle of the last token that was matched.
   --
   --  Raises Programmer_Error when the last token was read from the
   --  lookahead queue.
   --------------------------------------------------------------------------
   function Last_Recognizer (Analyzer : in Source) return Recognizer_Handle is abstract;

private
   type Instance is new OpenToken.Token.Instance with record
      ID    : Token_ID;
      Build : Action;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Class, Handle);

end OpenToken.Token.Enumerated;
