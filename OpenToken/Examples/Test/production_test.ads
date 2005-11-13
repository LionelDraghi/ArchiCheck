-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-- $Log: production_test.ads,v $
-- Revision 1.1  2000/08/12 21:17:04  Ted
-- initial version
--
--
-------------------------------------------------------------------------------

with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Production;
with OpenToken.Production.List;

-------------------------------------------------------------------------------
-- Library-level declarations for the yest driver for the token list handling
-- code.
-------------------------------------------------------------------------------
package Production_Test is

   type Token_IDs is (Int_ID, Real_ID, String_ID, Keyword_ID, Expression_ID, Literal_ID);

   package Master_Token is new OpenToken.Token.Enumerated(Token_IDs);
   package Tokenizer is new Master_Token.Analyzer(Keyword_ID);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal(Token_List);
   package Production is new OpenToken.Production(Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

end Production_Test;
