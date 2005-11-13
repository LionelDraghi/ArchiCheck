-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: opentoken-production-parser.ads,v $
-- Revision 1.2  2000/08/06 23:44:17  Ted
-- Fix to work w/ new package hierarchy
--
-- Revision 1.1  2000/01/27 20:52:45  Ted
-- Abstract type for parsers
--
--
--
-------------------------------------------------------------------------------

with OpenToken.Production.List;
with OpenToken.Token.Enumerated.Analyzer;

-------------------------------------------------------------------------------
-- This package provides an interface for a parser for grammars defined by a
-- production list. There are many possible different methods for parsing.
-------------------------------------------------------------------------------
generic
   with package Production_List is new OpenToken.Production.List;
   with package Tokenizer is new Token.Analyzer(<>);
package OpenToken.Production.Parser is

   type Instance is abstract tagged private;

   Parse_Error  : exception;

   ----------------------------------------------------------------------------
   -- Create a new parser from the given grammar with the given token analyzer.
   ----------------------------------------------------------------------------
   function Generate (Grammar  : in Production_List.Instance;
                      Analyzer : in Tokenizer.Instance) return Instance is abstract;

   ----------------------------------------------------------------------------
   -- Attempt a parse. This routine will return when the grammar indicates the
   -- first production has been parsed. (or an exception is raised)
   ----------------------------------------------------------------------------
   procedure Parse (Parser : in out Instance) is abstract;

private
   type Instance is abstract tagged record
      Analyzer : Tokenizer.Instance;
   end record;
end OpenToken.Production.Parser;
