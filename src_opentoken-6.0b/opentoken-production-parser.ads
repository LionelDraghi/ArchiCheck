-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2010, 2012, 2013, 2014 Stephe Leake
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

-------------------------------------------------------------------------------
--  This package provides an interface for a parser for grammars defined by a
--  production list. There are many possible different methods for parsing.
-------------------------------------------------------------------------------
with OpenToken.Text_Feeder;
with OpenToken.Token.Enumerated.Analyzer;
generic
   with package Tokenizer is new Token.Analyzer;
package OpenToken.Production.Parser is

   subtype Nonterminal_ID is Token.Token_ID range Token.Token_ID'Succ (Token.Last_Terminal) .. Token.Token_ID'Last;

   type Instance is abstract tagged record
      Analyzer : Tokenizer.Handle;
   end record;

   ----------------------------------------------------------------------------
   --  Attempt a parse. This routine will return when the grammar indicates the
   --  first production has been parsed. (or an exception is raised)
   ----------------------------------------------------------------------------
   procedure Parse (Parser : in out Instance) is abstract;

   --------------------------------------------------------------------------
   --  Reset the internal Analyzer. Appropriate if the Text_Feeder's
   --  input has changed.
   --------------------------------------------------------------------------
   procedure Reset (Parser : in out Instance);

   --------------------------------------------------------------------------
   --  Set the parser's text feeder. Raises Parse_Error if current
   --  text buffer is not empty.
   --------------------------------------------------------------------------
   procedure Set_Text_Feeder (Parser : in out Instance; Feeder : in Text_Feeder.Text_Feeder_Ptr);

   ------------------------------------------------------------------------
   --  Discard text in Parser.Analyzer's internal buffer. Do this when
   --  a parse error is encountered, and you want to start over.
   ------------------------------------------------------------------------
   procedure Discard_Buffered_Text (Parser : in out Instance);

   ------------------------------------------------------------------------
   --  True if all text buffers are empty, and text feeder reports end
   --  of text.
   function End_Of_Text (Parser : in Instance) return Boolean;

   ----------------------------------------------------------------------------
   --  Returns the current text line at which processing will resume. This is
   --  particularly useful for printing error messages when syntax errors are
   --  detected.
   ----------------------------------------------------------------------------
   function Line (Parser : in Instance) return Natural;

   ----------------------------------------------------------------------------
   --  Returns the current text column at which processing will resume. This is
   --  particularly useful for printing error messages when syntax errors are
   --  detected.
   ----------------------------------------------------------------------------
   function Column (Parser : in Instance) return Natural;

end OpenToken.Production.Parser;
