-------------------------------------------------------------------------------
--
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
--  This package provides a type and operations for building grammar
--  productions.
-------------------------------------------------------------------------------
package body OpenToken.Production is

   function "+" (Tokens : in Token_List.Instance;
                 Action : in Nonterminal.Synthesize
                 ) return Right_Hand_Side is
   begin
      return (Tokens => Tokens,
              Action => Action
              );
   end "+";
   function "+" (Tokens : in Token.Class;
                 Action : in Nonterminal.Synthesize
                 ) return Right_Hand_Side is
   begin
      return (Tokens => Token_List.Only (Tokens),
              Action => Action
              );
   end "+";

   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Right_Hand_Side
                 ) return Instance is
   begin
      return (LHS => LHS,
              RHS => RHS
              );
   end "<=";

   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Right_Hand_Side
                  ) return Instance is
   begin
      return (LHS => new Nonterminal.Class'(LHS),
              RHS => RHS
              );
   end "<=";

   ----------------------------------------------------------------------------
   --  Build productions using the default synthesization routines.
   ----------------------------------------------------------------------------
   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Token_List.Instance
                  ) return Instance is
   begin
      return (LHS => LHS,
              RHS => RHS + Nonterminal.Synthesize_Default
              );
   end "<=";

   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Token_List.Instance
                  ) return Instance is
   begin
      return (LHS => new Nonterminal.Class'(LHS),
              RHS => RHS + Nonterminal.Synthesize_Default
              );
   end "<=";

   ----------------------------------------------------------------------------
   --  Production building operartors using tokens
   ----------------------------------------------------------------------------
   function "<=" (LHS : in Nonterminal.Handle;
                  RHS : in Token.Class
                  ) return Instance is
   begin
      return (LHS => LHS,
              RHS => RHS + Nonterminal.Synthesize_Default
              );
   end "<=";


   function "<=" (LHS : in Nonterminal.Class;
                  RHS : in Token.Class
                 ) return Instance is
   begin
      return (LHS => new Nonterminal.Class'(LHS),
              RHS => RHS + Nonterminal.Synthesize_Default
              );
   end "<=";

end OpenToken.Production;

