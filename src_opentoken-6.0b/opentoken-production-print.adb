--  Abstract :
--
--  See spec
--
--  Copyright (C) 2002, 2014 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with Ada.Text_IO; use Ada.Text_IO;
package body OpenToken.Production.Print is

   procedure Print (Item : in Instance)
   is begin
      Put ("(" & Token.Token_Image (Token.ID (Item.LHS.all)) & " <= ");
      Print (Item.RHS);
      Put (")");
   end Print;

   procedure Print (Item : in Right_Hand_Side)
   is
      use type Nonterminal.Synthesize;
   begin
      Token_List.Print (Item.Tokens);
      Put (", Action => ");
      Print_Action (Item.Action);
   end Print;

end OpenToken.Production.Print;
