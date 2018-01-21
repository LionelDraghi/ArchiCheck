-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2014 Stephen Leake
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

with OpenToken.Text_Feeder;
package HTML_Lexer.Task_Safe is

   -----------------------------------------------------------------------
   --  This is a lexical analyser for the HTML language.
   --
   --  All state data is stored in the Lexer_Type object, so it is
   --  task safe.
   --
   --   <Tag Attribute=Value Attribute="String"> Text with &entity; </Tag>
   ------------------------------------------------------------------------

   type Lexer_Type is limited private;

   procedure Initialize
     (Lexer        : in out Lexer_Type;
      Input_Feeder : access OpenToken.Text_Feeder.Instance'Class);

   procedure Next_Token
     (Lexer : in out Lexer_Type;
      Token :    out HTML_Token);

private

   type Lexer_Type is record
      --  We switch syntax during parsing. The Syntax structure
      --  contains state, so each lexer needs its own copy.
      Text_Syntax : Tokenizer.Syntax;
      Tag_Syntax  : Tokenizer.Syntax;
      Analyzer    : Tokenizer.Handle;
   end record;

end HTML_Lexer.Task_Safe;
