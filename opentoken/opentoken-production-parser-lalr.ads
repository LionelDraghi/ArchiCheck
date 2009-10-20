-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003 Stephe Leake
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
--  This package provides an implementation of a LALR (Look-Ahead
--  Left-to-right scanning Rightmost-deriving) parser for grammars
--  defined by a production list. This is probably the most popular
--  method due to it being a good trade-off between the amount of
--  grammars handled and the size of its parse table.
-----------------------------------------------------------------------------
generic
package OpenToken.Production.Parser.LALR is

   type Instance is new OpenToken.Production.Parser.Instance with private;

   overriding function Generate
     (Grammar  : in Production_List.Instance;
      Analyzer : in Tokenizer.Instance;
      Trace    : in Boolean                  := False)
     return Instance;

   overriding procedure Parse (Parser : in out Instance);

   --------------------------------------------------------------------------
   --  Free any resources used by the given parser. It will be invalid
   --  after this call.
   --------------------------------------------------------------------------
   procedure Cleanup (Parser : in out Instance) is null;

   --------------------------------------------------------------------------
   --  This routine displays the parse table for the parser to
   --  Ada.Text_IO.Current_Output. This may be useful for debugging
   --  grammars (or, heaven forbid, the parser itself).
   --------------------------------------------------------------------------
   procedure Print_Table (Parser : in Instance);

   procedure Set_Trace (Parser : in out Instance; Enabled : in Boolean);
   --  If Enabled, print trace of state transitions to
   --  Ada.Text_IO.Standard_Error when Parse is called.

private

   --  Type for parser states
   type State_Index is new Integer;

   type Action_Node;
   type Action_Node_Ptr is access Action_Node;

   type Reduction_Node;
   type Reduction_Node_Ptr is access Reduction_Node;

   type Parse_State is record
      Action_List    : Action_Node_Ptr;
      Reduction_List : Reduction_Node_Ptr;
   end record;

   type Parse_Table is array (State_Index range <>) of Parse_State;

   type Parse_Table_Ptr is access Parse_Table;

   type Instance is new OpenToken.Production.Parser.Instance with record
      Table : Parse_Table_Ptr;
      Trace : Boolean := False;
   end record;

end OpenToken.Production.Parser.LALR;
