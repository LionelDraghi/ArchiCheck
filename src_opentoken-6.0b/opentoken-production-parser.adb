-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2010, 2012, 2014 Stephe Leake
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

with Ada.Exceptions;
package body OpenToken.Production.Parser is

   procedure Reset (Parser : in out Instance)
   is begin
      Parser.Analyzer.Reset;
   end Reset;

   procedure Set_Text_Feeder (Parser : in out Instance; Feeder : in Text_Feeder.Text_Feeder_Ptr)
   is begin
      if Parser.Analyzer.End_Of_Buffered_Text then
         Parser.Analyzer.Set_Text_Feeder (Feeder);
      else
         Ada.Exceptions.Raise_Exception
           (Parse_Error'Identity, "Tokenizer not at end of buffered text");
      end if;
   end Set_Text_Feeder;

   procedure Discard_Buffered_Text (Parser : in out Instance)
   is begin
      Parser.Analyzer.Discard_Buffered_Text;
   end Discard_Buffered_Text;

   function End_Of_Text (Parser : in Instance) return Boolean
   is begin
      return Parser.Analyzer.End_Of_Text;
   end End_Of_Text;

   function Line (Parser : in Instance) return Natural is
   begin
      return Parser.Analyzer.Line;
   end Line;

   function Column (Parser : in Instance) return Natural is
   begin
      return Parser.Analyzer.Column;
   end Column;

end OpenToken.Production.Parser;
