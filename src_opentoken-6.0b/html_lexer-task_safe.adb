-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2012, 2014 Stephen Leake
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
package body HTML_Lexer.Task_Safe is

   procedure Initialize
     (Lexer        : in out Lexer_Type;
      Input_Feeder : access OpenToken.Text_Feeder.Instance'Class)
   is begin
      Lexer :=
        (Text_Syntax => Text_Syntax,
         Tag_Syntax  => Tag_Syntax,
         Analyzer    => Tokenizer.Initialize
           (Text_Syntax,
            Default  => Bad_Token,
            Feeder   => OpenToken.Text_Feeder.Text_Feeder_Ptr (Input_Feeder)));
   end Initialize;

   procedure Next_Token
     (Lexer : in out Lexer_Type;
      Token :    out HTML_Token)
   is begin
      Lexer.Analyzer.Find_Next;

      Token :=
        (Name   => Lexer.Analyzer.ID,
         Lexeme => Ada.Strings.Unbounded.To_Unbounded_String (Lexer.Analyzer.Lexeme),
         Line   => Lexer.Analyzer.Line,
         Column => Lexer.Analyzer.Column);

      case Token.Name is
      when Start_Tag_Opener | End_Tag_Opener =>
         Lexer.Analyzer.Set_Syntax (Lexer.Tag_Syntax);

      when Tag_Closer =>
         Lexer.Analyzer.Set_Syntax (Lexer.Text_Syntax);

      when others =>
         null;

      end case;
   end Next_Token;

end HTML_Lexer.Task_Safe;
