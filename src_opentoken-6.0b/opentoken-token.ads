-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014 Stephe Leake
--  Copyright (C) 1999, 2000 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
--  This package defines an abstract token type, for use by this
--  facility. It also defines a parse operation and a token source
--  type, for use by recursive decent parsers.
-----------------------------------------------------------------------------

limited with OpenToken.Token.Linked_List;
package OpenToken.Token is

   type Instance is abstract tagged private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   --  Return a string for debug messages
   function Image (Token : in Instance) return String is abstract;

   type Source is abstract tagged private;

   subtype Source_Class is Source'Class;

   type Source_Handle is access all Source_Class;

   Default_Lookahead : Integer := 1;

   --------------------------------------------------------------------
   --  Verify that token in Analyzer matches the token Match, possibly
   --  take some action.
   --
   --  If not Actively, this should determine as quickly as possible
   --  whether the parse would fail or succeed, and raise Parse_Error
   --  with no message if it would fail. Depending on the grammar
   --  design, only checking the current Analyzer token would be
   --  enough, but it may be necessary to look ahead. In that case,
   --  Default_Lookahead should be used to specify the default
   --  lookahead count, and the user should be able to override it for
   --  specific tokens. When called in this way, a higher level parse
   --  is choosing between options.
   --
   --  If Actively, upon successful completion, update Match with the
   --  value of the token, and advance Analyzer across all matched
   --  tokens.
   --
   --  If not Actively, Match is unchanged, and Analyzer is not
   --  advanced.
   --
   --  Match is 'access' to match Expecting, which needs to store a
   --  pointer to it in some cases.
   ------------------------------------------------------------------
   procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True)
      is abstract;

   --------------------------------------------------------------------------
   --  Locate the next token.
   --
   --  If Look_Ahead is set, the next token after the current one will
   --  be returned, but the current one will not be discarded; they
   --  are saved in the lookahead queue. Subsequent Look_Ahead calls
   --  will return later and later tokens.
   --
   --  The very first call to Find_Next, immediately after Analyzer is
   --  created, cannot have Look_Ahead True. No real parser needs
   --  Look_Ahead True on the first call in a real parse, and this lets
   --  the Analyzer assume there is one actively recognized token to
   --  start the lookahead queue with.
   --
   --  Raises Syntax_Error with an appropriate message if no token
   --  is found and there is no default token.
   --------------------------------------------------------------------------
   procedure Find_Next
     (Analyzer   : in out Source;
      Look_Ahead : in     Boolean := False)
      is abstract;

   ----------------------------------------------------------------------------
   --  Returns the last token that was matched.
   ----------------------------------------------------------------------------
   function Get (Analyzer : in Source) return Class is abstract;

   type Queue_Mark is abstract tagged limited null record;

   --------------------------------------------------------------------
   --  Mark a point in the lookahead queue.
   --------------------------------------------------------------------
   function Mark_Push_Back (Analyzer : in Source) return Queue_Mark'Class is abstract;

   ------------------------------------------------------------------
   --  Restore the input point in the lookahead queue. Subsequent
   --  calls to Get will return the token that was current when Mark
   --  was set. This allows a recursive descent parser to backtrack.
   ------------------------------------------------------------------
   procedure Push_Back (Analyzer : in out Source; Mark : in Queue_Mark'Class) is abstract;

   ----------------------------------------------------------------------
   --  If Name is not "", set the token name to Name.
   ----------------------------------------------------------------------
   procedure Set_Name (Token : in out Class; Name : in String);

   ----------------------------------------------------------------------
   --  Return the name of Token, for error messages
   ----------------------------------------------------------------------
   function Name (Token : in Instance) return String;

   function Has_Name (Token : in Instance) return Boolean;

   ----------------------------------------------------------------------
   --  Dispatching calls to Name
   ----------------------------------------------------------------------
   function Name_Dispatch (Token : in Class) return String;
   function Name_Dispatch (Token : access constant Instance'Class) return String;

   ----------------------------------------------------------------------
   --  Add expected tokens to List, for error messages
   ----------------------------------------------------------------------
   procedure Expecting (Token : access Instance; List : in out Linked_List.Instance);

   ------------------------------------------------------------------
   --  We _don't_ define a 'Print' procedure for tokens, even though
   --  that might be useful in debugging recursive descent parsers.
   --  The problem is that grammars are often recursive, which leads
   --  to infinite loops in Print, and dealing with such loops is too
   --  hard.

private
   type Instance is abstract tagged record
      Name : access String;
   end record;

   type Source   is abstract tagged null record;

end OpenToken.Token;
