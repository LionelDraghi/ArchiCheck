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

package body HTML_Lexer.Task_Unsafe is

   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Text_Syntax, Default => Bad_Token);

   procedure Initialize (Input_Feeder : in OpenToken.Text_Feeder.Text_Feeder_Ptr) is
   begin
      Analyzer.Set_Text_Feeder (Input_Feeder);
   end Initialize;

   function Next_Token return HTML_Token is
      Result : HTML_Token;
   begin
      Analyzer.Find_Next;

      Result :=
        (Name   => Analyzer.ID,
         Lexeme => Ada.Strings.Unbounded.To_Unbounded_String (Analyzer.Lexeme),
         Line   => Analyzer.Line,
         Column => Analyzer.Column);

      case Result.Name is
      when Start_Tag_Opener | End_Tag_Opener =>
         Analyzer.Set_Syntax (Tag_Syntax);

      when Tag_Closer =>
         Analyzer.Set_Syntax (Text_Syntax);

      when others =>
         null;
      end case;
      return Result;
   end Next_Token;

end HTML_Lexer.Task_Unsafe;
