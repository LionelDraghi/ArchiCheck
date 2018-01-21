-------------------------------------------------------------------------------
--
--  Copyright (C) 2002, 2003, 2009, 2012 - 2014 Stephe Leake
--  Copyright (C) 1999 FlightSafety International and Ted Dennison
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
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package implements a mostly full-strength tokenizer (or
--  lexical analyizer).
--
--  To use it, create a function to feed text strings into the
--  Analyzer. Create an enumerated type of all the tokens you want to
--  recognize. Instantiate this package with the function and the
--  enumerated type.
--
--  Next, define a token subclass for each token in Tokens. Then
--  create a Syntax which matches up the tokens to their appropriate
--  token class and pass it into Set_Syntax.
--
--  Once that is done, you may repeatedly call Get_Next to get tokens.
-----------------------------------------------------------------------------

with OpenToken.Recognizer;
generic
package OpenToken.Token.Enumerated.Analyzer is

   type Token_Array_Boolean is array (Token_ID range First_Terminal .. Token_ID'Last) of Boolean;

   --  Descriptor for what an individual token in this language looks
   --  like. Also provides storage for Lexeme and Recognizer from
   --  recognized tokens. This is required by lookahead; the lexeme
   --  and recognizer are only available when the token is recognized
   --  in the input stream, not later when it is read from the
   --  lookahead queue. Copies of the recognized token are pushed onto
   --  the lookahead queue, after Create is called.
   type Recognizable_Token is record
      Recognizer   : Recognizer_Handle;
      Token_Handle : Handle;
   end record;

   --  The syntax of a language, which is defined by the set of non-reporting and Terminal tokens.
   subtype Syntax_ID is Token_ID range Token_ID'First .. Last_Terminal;
   type Syntax is array (Syntax_ID) of Recognizable_Token;

   type Instance (Max_Buffer_Size : Integer) is new Source with private;
   type Handle is access all Instance;

   --  Need to revisit token definitions or raise Max_String_Length
   Token_Too_Long : exception;

   --------------------------------------------------------------------------
   --  Return a new recognizable token, using the given token
   --  values. This is a convienence routine for more easily creating
   --  Syntaxes. It will dynamically allocate the memory for the
   --  recognizer and token.
   --------------------------------------------------------------------------
   function Get
     (Recognizer : in OpenToken.Recognizer.Class;
      New_Token  : in OpenToken.Token.Enumerated.Class := Get)
     return Recognizable_Token;

   function Null_Analyzer return Instance;
   --  no input stream; used when grammar will be used for something other than parsing.

   ----------------------------------------------------------------------------
   --  Return an Analyzer with the given syntax and text feeder.
   ----------------------------------------------------------------------------
   function Initialize
     (Language_Syntax : in Syntax;
      Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                               := 1024;
      First_Column    : in Integer                               := 1)
     return Handle;
   function Initialize
     (Language_Syntax : in Syntax;
      Default         : in Terminal_ID;
      Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                               := 1024;
      First_Column    : in Integer                               := 1)
     return Handle;

   ----------------------------------------------------------------------
   --  Return name of ID token in Analyzer.Syntax
   ----------------------------------------------------------------------
   function Name (Analyzer : in Instance; ID : in Token_ID) return String;

   --------------------------------------------------------------------
   --  Reset Analyzer, to start finding tokens. This is appropriate
   --  when the Feeder text has been changed.
   --------------------------------------------------------------------
   procedure Reset (Analyzer : in out Instance);

   ----------------------------------------------------------------------------
   --  Set the Analyzer's syntax to the given value.
   --
   --  Due to the accessability rules of Ada, you cannot create syntax
   --  objects in which the component tokens are declared at a deeper
   --  dynamic scope than the instantiation of this package using
   --  'access on the tokens. 'Unchecked_Access is safe to use as long
   --  as the Analyzer does not have a longer lifetime than its
   --  tokens.
   --
   --  Note that the Syntax structure contains pointers to
   --  recognizers, which have dynamic state. Set_Syntax does a simple
   --  copy of the array, not a deep copy of the recognizer objects.
   --  Therefore this Analyzer will share those recognizers with any
   --  other Analyzer using the same syntax, which can happen in a
   --  multi-threaded system.
   --
   --  We could make Syntax Limited_Controlled and provide a deep copy
   --  in Adjust. But that would significantly complicate creating a
   --  syntax, and make it expensive to switch syntaxes during a parse
   --  (as HTML_Lexer does).
   --
   ----------------------------------------------------------------------
   procedure Set_Syntax (Analyzer : in out Instance; Language_Syntax : in Syntax);

   ----------------------------------------------------------------------------
   --  Set the analyzer's text feeder.
   ----------------------------------------------------------------------------
   procedure Set_Text_Feeder (Analyzer : in out Instance; Feeder : in OpenToken.Text_Feeder.Text_Feeder_Ptr);

   ------------------------------------------------------------------------
   --  True if Analyzer's internal buffer is empty, and
   --  Analyzer.Text_Feeder reports End_Of_Text.
   function End_Of_Text (Analyzer : in Instance) return Boolean;

   ------------------------------------------------------------------------
   --  True if Analyzer's internal buffer is empty.
   function End_Of_Buffered_Text (Analyzer : in Instance) return Boolean;

   --------------------------------------------------------------------------
   --  Discard text in Analyzer's internal buffer. Do this when a
   --  parse error is encountered, and you want to start over.
   --------------------------------------------------------------------------
   procedure Discard_Buffered_Text (Analyzer : in out Instance);

   ----------------------------------------------------------------------------
   --  Set the analyzer's default token to the given ID.
   --
   --  If Find_Next can't find a matching token, it will set Token to
   --  this token id, instead of raising syntax error. The Lexeme in
   --  this situation will be contain all the contiguous characters
   --  that fail to match an token. In practice this will be much less
   --  efficient than an "error" token that explicitly matches
   --  unmatchable strings. But often those are quite difficult to
   --  construct. The default token will be checked for legitimate
   --  matches. If this is not the behavior you want, it would be best
   --  to use a token that can't match any legitimate string (eg:
   --  Opentoken.Recognizer.Nothing)
   --------------------------------------------------------------------------
   procedure Set_Default
     (Analyzer : in out Instance;
      Default  : in     Terminal_ID);

   --------------------------------------------------------------------------
   --  Reset the analyzer to have *no* default token ID. If Find_Next
   --  doesn't find a matching token, Syntax_Error will be raised.
   --------------------------------------------------------------------------
   procedure Unset_Default (Analyzer : in out Instance);

   overriding procedure Find_Next
     (Analyzer   : in out Instance;
      Look_Ahead : in     Boolean := False);

   type Queue_Mark is new Token.Queue_Mark with private;

   overriding function Mark_Push_Back (Analyzer : in Instance) return Token.Queue_Mark'Class;
   overriding procedure Push_Back (Analyzer : in out Instance; Mark : in Token.Queue_Mark'Class);

   --------------------------------------------------------------------------
   --  Returns the current text line number at which processing will resume.
   --  This is particularly useful for printing error messages when
   --  syntax errors are detected.
   --------------------------------------------------------------------------
   function Line (Analyzer : in Instance) return Natural;

   --------------------------------------------------------------------------
   --  Returns the current text column number at which processing will
   --  resume. This is particularly useful for printing error messages
   --  when syntax errors are detected. First column number is given
   --  in Initialize.
   --------------------------------------------------------------------------
   function Column (Analyzer : in Instance) return Natural;

   --------------------------------------------------------------------------
   --  Returns True if the next token will be at the start of its text
   --  line. The main purpose of this routine is to assist in writing
   --  recognizers for tokens that must start a line.
   --------------------------------------------------------------------------
   function First_Column (Analyzer : in Instance) return Boolean;

   --------------------------------------------------------------------------
   --  Returns the column at which the the next token starts on its
   --  text line. The main purpose of this routine is to assist in
   --  writing recognizers for tokens that must start on a specific
   --  column
   --------------------------------------------------------------------------
   function Next_Token_Column (Analyzer : in Instance) return Integer;

   overriding function Get (Analyzer : in Instance) return OpenToken.Token.Class;

   ----------------------------------------------------------------------------
   --  Returns the last token ID that was matched.
   ----------------------------------------------------------------------------
   function ID (Analyzer : in Instance) return Terminal_ID;

   overriding function Lexeme (Analyzer : in Instance) return String;

   --------------------------------------------------------------------------
   --  Returns the position of the start and end of the last token
   --  that was matched, in the internal buffer.
   --------------------------------------------------------------------------
   function Bounds (Analyzer : in Instance) return Buffer_Range;

   overriding function Last_Recognizer (Analyzer : in Instance) return Recognizer_Handle;

private

   type Token_List_Node;
   type Token_List_Node_Pointer is access Token_List_Node;

   --  Visible for unit tests
   type Token_List_Node is record
      Token_Handle : OpenToken.Token.Enumerated.Handle;
      Prev         : Token_List_Node_Pointer;
      Next         : Token_List_Node_Pointer;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Token_List_Node, Token_List_Node_Pointer);

   --  Put all the Analyzer's state information in here, so there can
   --  be several Analyzers running at once.
   type Instance (Max_Buffer_Size : Integer) is new Source with record

      --  User-settable attributes
      Syntax_List   : Syntax;
      Has_Default   : Boolean := False;
      Default_Token : Terminal_ID;
      First_Column  : Integer;

      --  User-gettable attributes
      Line              : Natural := 1;
      Column            : Natural := 1;
      Lexeme_Head       : Natural := 1;
      Lexeme_Tail       : Natural := 0;
      Lexeme_Source_Pos : Natural := 1;
      Last_Token_ID     : Terminal_ID;

      Read_From_Lookahead : Boolean;

      --  Internal state information
      Buffer                 : String (1 .. Max_Buffer_Size); --  FIXME: allow reallocate in Reset
      Buffer_Head            : Natural := 1;
      Buffer_Tail            : Natural := 0;
      Buffer_Size            : Natural := 0; -- = tail - head, wrapped; 0 if empty
      Buffer_Head_Source_Pos : Natural := 1;

      Next_Line    : Natural := 1;
      Next_Column  : Natural := 1;

      Lookahead_Queue : Token_List_Node_Pointer; --  Read from here or text source when Look_Ahead is false
      Lookahead_Head  : Token_List_Node_Pointer; --  Read from here or text source when Look_Ahead is true
      Lookahead_Tail  : Token_List_Node_Pointer; --  Most recent token read from text source with Look_Ahead true
      Lookahead_Count : Integer;
      Max_Lookahead   : Integer;
   end record;

   type Queue_Mark is new Token.Queue_Mark with record
      Head : Token_List_Node_Pointer;
      Tail : Token_List_Node_Pointer;
   end record;

end OpenToken.Token.Enumerated.Analyzer;
