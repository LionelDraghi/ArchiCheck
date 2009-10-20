-------------------------------------------------------------------------------
--
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
-------------------------------------------------------------------------------

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Token.Enumerated.List;

-------------------------------------------------------------------------------
--  This package declares a type for designating an integer literal.
-------------------------------------------------------------------------------
generic
   with package Token           is new OpenToken.Token.Enumerated (<>);
   with package Token_List      is new Token.List;
   with package Nonterminal     is new Token.Nonterminal (Token_List);
   with package Integer_Literal is new Token.Integer_Literal;
package Simple_Integer_Token is

   type Instance is new Nonterminal.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Get an integer literal token with the given ID and value.
   ----------------------------------------------------------------------------
   function Get (ID     : in Token.Token_ID;
                 Value  : in Integer := 0
                ) return Instance'Class;

   ----------------------------------------------------------------------------
   --  Return the value of the given integer token.
   ----------------------------------------------------------------------------
   function Value (Subject : in Instance) return Integer;

   ----------------------------------------------------------------------------
   --  Overriding of the default copy synthesization rouine. This routine
   --  accepts all token lists whose first element is in Instance'Class, or in
   --  Integer_Literal.Instance'Class.
   ----------------------------------------------------------------------------
   overriding procedure Synthesize_By_Copying
     (New_Token : out Instance;
      Source    : in  Token.Instance'Class;
      To_ID     : in  Token.Token_ID);

   ----------------------------------------------------------------------------
   --  Routines to synthesize tokens by performing integer ops on the list they
   --  reduce from.
   --
   --  They are valid for any token list whose first and third members are in
   --  Instance'Class.
   ----------------------------------------------------------------------------
   Add_Integers      : constant Nonterminal.Synthesize;
   Multiply_Integers : constant Nonterminal.Synthesize;

   ----------------------------------------------------------------------------
   --  Routine to synthesize a simple integer token from the second token on the
   --  production's right hand side.
   ----------------------------------------------------------------------------
   Synthesize_Second : constant Nonterminal.Synthesize;

   --------------------------------------------------------------------------
   --  Routine to print the value of the given Single interger token.
   --------------------------------------------------------------------------
   procedure Print_Value (New_Token : out Nonterminal.Class;
                          Source    : in  Token_List.Instance'Class;
                          To_ID     : in  Token.Token_ID);

private
   type Instance is new Nonterminal.Instance with record
      Value : Integer;
   end record;

   ----------------------------------------------------------------------------
   --  Implementations of routines to synthesize tokens by performing integer
   --  ops on the list they  reduce from.
   --
   --  They are valid for any token list whose first and third members are in
   --  Instance'Class.
   ----------------------------------------------------------------------------
   procedure Synthesize_Add (New_Token : out Nonterminal.Class;
                             Source    : in  Token_List.Instance'Class;
                             To_ID     : in  Token.Token_ID);
   procedure Synthesize_Multiply (New_Token : out Nonterminal.Class;
                                  Source    : in  Token_List.Instance'Class;
                                  To_ID     : in  Token.Token_ID);

   ----------------------------------------------------------------------------
   --  Routine to synthesize a simple integer token from the second token on the
   --  production's right hand side.
   ----------------------------------------------------------------------------
   procedure Synthesize_From_Second_Argument (New_Token : out Nonterminal.Class;
                                              Source    : in  Token_List.Instance'Class;
                                              To_ID     : in  Token.Token_ID);

   Add_Integers      : constant Nonterminal.Synthesize := Synthesize_Add'Access;
   Multiply_Integers : constant Nonterminal.Synthesize := Synthesize_Multiply'Access;
   Synthesize_Second : constant Nonterminal.Synthesize := Synthesize_From_Second_Argument'Access;

end Simple_Integer_Token;


