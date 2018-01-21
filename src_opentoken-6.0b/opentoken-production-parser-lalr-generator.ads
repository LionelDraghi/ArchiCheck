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
--  This package provides the generalized LALR parse table generator.

pragma License (Modified_GPL);

with OpenToken.Production.List;
with OpenToken.Production.Parser.LRk_Item;
generic
   Token_Image_Width : Integer;
   with package Production_List is new OpenToken.Production.List;
package OpenToken.Production.Parser.LALR.Generator is

   package LRk is new OpenToken.Production.Parser.LRk_Item (Unknown_State_Index, Unknown_State, 1, Production_List);

   function Generate
     (Grammar                  : in Production_List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr;
   --  Generate a generalized LALR parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  If Trace, output debug info about generation process. We don't
   --  use OpenToken.Trace here; we often want to see a trace of the
   --  parser execution without the parser generation.
   --
   --  Unless Ignore_Unused_Tokens is True, raise Grammar_Error if
   --  there are unused tokens.
   --
   --  Unless Ignore_Unknown_Conflicts is True, raise Grammar_Error if there
   --  are unknown conflicts.

   ----------
   --  Visible for unit tests

   procedure Put (State : in Parse_State);

   procedure Fill_In_Lookaheads
     (Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Kernels              : in out LRk.Item_Set_List;
      Accept_State         : in     State_Index;
      Used_Tokens          : in out Tokenizer.Token_Array_Boolean;
      Trace                : in     Boolean);

   procedure Add_Actions
     (Kernel               : in     LRk.Item_Set_Ptr;
      Accept_State         : in     State_Index;
      Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean);

end OpenToken.Production.Parser.LALR.Generator;
