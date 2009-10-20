-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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

with OpenToken.Token.Linked_List;

-----------------------------------------------------------------------------
--  This package defines a reusable token for a simple sequence of
--  tokens. These a quite easy to create yourself, of course. But
--  having a prebuilt one allows you to easily use it in constructors
--  for other tokens.
--
-------------------------------------------------------------------------------
package OpenToken.Token.Sequence is

   type Instance is new Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Create a token sequence from a pair of token handles.
   ----------------------------------------------------------------------------
   function "&" (Left  : access OpenToken.Token.Class;
                 Right : access OpenToken.Token.Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token sequence from a token handle and a token sequence.
   ----------------------------------------------------------------------------
   function "&" (Left  : access OpenToken.Token.Class;
                 Right : in     Instance) return Instance;
   function "&" (Left  : in     Instance;
                 Right : access OpenToken.Token.Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token sequence from a pair of token sequences.
   ----------------------------------------------------------------------------
   function "&" (Left  : in Instance;
                 Right : in Instance) return Instance;

   ----------------------------------------------------------------------------
   --  Return a newly allocated instance which is a copy of the given instance.
   ----------------------------------------------------------------------------
   function New_Instance (Old_Instance : in Instance) return Handle;

   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean;

   ----------------------------------------------------------------------------
   --  This routine is called when an entire sequence has been actively
   --  parsed.
   ----------------------------------------------------------------------------
   procedure Build (Match : in out Instance;
                    Using : in     Token.Linked_List.Instance) is null;

private
   type Instance is new Token.Instance with record
      Members : Token.Linked_List.Instance;
   end record;

end OpenToken.Token.Sequence;
