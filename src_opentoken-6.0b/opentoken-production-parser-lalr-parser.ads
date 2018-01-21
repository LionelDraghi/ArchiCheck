-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2009, 2010, 2013, 2014 Stephe Leake
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
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package provides the LALR parser.

with OpenToken.Production.Parser.LALR.Parser_Lists;
generic
   First_Parser_Label : in Integer;
   with package Parser_Lists is new OpenToken.Production.Parser.LALR.Parser_Lists (First_Parser_Label);
package OpenToken.Production.Parser.LALR.Parser is

   type Instance is new OpenToken.Production.Parser.Instance with record
      Table                : Parse_Table_Ptr;
      Max_Parallel         : Integer;
      Terminate_Same_State : Boolean;
   end record;

   function Initialize
     (Analyzer             : in Tokenizer.Handle;
      Table                : in Parse_Table_Ptr;
      Max_Parallel         : in Integer   := 15;
      Terminate_Same_State : in Boolean   := False)
     return Instance;

   overriding procedure Parse (Parser : in out Instance);

end OpenToken.Production.Parser.LALR.Parser;
