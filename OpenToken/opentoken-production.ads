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
-- $Log: opentoken-production.ads,v $
-- Revision 1.2  2000/08/06 23:44:44  Ted
-- Fix to work w/ new package hierarchy
--
-- Revision 1.1  2000/01/27 20:50:39  Ted
-- Packages to support grammar productions
--
--
--
-------------------------------------------------------------------------------

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;

pragma Elaborate_All (OpenToken.Token.Enumerated);

-------------------------------------------------------------------------------
-- This package provides a type and operations for building grammar
-- productions.
-------------------------------------------------------------------------------
generic
   with package Token is new OpenToken.Token.Enumerated(<>);
   with package Token_List is new Token.List;
   with package Nonterminal  is new Token.Nonterminal (Token_List);
package OpenToken.Production is

   ----------------------------------------------------------------------------
   -- The Right Hand Side of a production is a token list "+"ed with a
   -- synthesization routine. For example:
   --     Number & Minus_Sign & Number + Nonterminal.Synthesize_First
   --
   type Right_Hand_Side is private;

   ----------------------------------------------------------------------------
   -- Create the right hand side of a production from a list of tokens and
   -- a synthesization routine. The synthesization routine will be called
   -- whenever the production is reduced by the parser.
   ----------------------------------------------------------------------------
   function "+" (Tokens : in Token_List.Instance;
                 Action : in Nonterminal.Synthesize
                 ) return Right_Hand_Side;
   function "+" (Tokens : in Token.Class;
                 Action : in Nonterminal.Synthesize
                 ) return Right_Hand_Side;

   ----------------------------------------------------------------------------
   -- A production instance consists of a nonterminal token instance "<=" ed to
   -- a Right Hand Side . For example:
   --    Subtraction <= Number & Minus_Sign & Number +
   --                   Nonterminal.Synthesize_Self
   --
   -- If a token list with no synthesization routine is used, the target
   -- non-terminal's default synthesization routine will be used. For example:
   --    Subtraction <= Number & Minus_Sign & Number
   -- will use Subtraction's default synthesization routine.
   --
   type Instance is private;

   type Handle is access all Instance;

   ----------------------------------------------------------------------------
   -- Production building operartors using Right Hand Sides
   ----------------------------------------------------------------------------
   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Right_Hand_Side
                 ) return Instance;

   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Right_Hand_Side
                 ) return Instance;

   ----------------------------------------------------------------------------
   -- Production building operartors using token lists
   ----------------------------------------------------------------------------
   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Token_List.Instance
                 ) return Instance;

   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Token_List.Instance
                 ) return Instance;

   ----------------------------------------------------------------------------
   -- Production building operartors using tokens
   ----------------------------------------------------------------------------
   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Token.Class
                 ) return Instance;

   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Token.Class
                 ) return Instance;

private

   type Right_Hand_Side is record
      Tokens : Token_List.Instance;
      Action : Nonterminal.Synthesize;
   end record;

   type Instance is record
      LHS : Nonterminal.Handle;
      RHS : Right_Hand_Side;
   end record;

end OpenToken.Production;

