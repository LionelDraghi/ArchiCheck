-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2014 Stephe Leake
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
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package defines a generic sequence of tokens, for recursive
--  descent parsers.
-----------------------------------------------------------------------------

with OpenToken.Token.Linked_List;
generic
   type Parent_Token is abstract new OpenToken.Token.Instance with private;
package OpenToken.Token.Sequence_Mixin is

   type Instance is new Parent_Token with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding
   function Image (Item : in Instance) return String;

   --------------------------------------------------------------------------
   --  A Build action specified in New_Instance or "+" is called when an
   --  entire sequence has been actively parsed. Using is the sequence
   --  of tokens.
   --------------------------------------------------------------------------
   type Action is access procedure
     (Match : in out Instance;
      Using : in     Token.Linked_List.Instance);

   -----------------------------------------------------------------------
   --  Create a token sequence from a pair of token handles.
   --
   --  If either is a sequence, it is included by reference; the
   --  member list is _not_ examined. Together with returning Instance
   --  rather than Handle, this allows for controlled recursion. It
   --  also requires the use of New_Selection or "+" to return an
   --  object compatible with Selection and other tokens, which has
   --  the effect of making it clear when recursion is desired.
   --
   --  These arguments must be 'access OpenToken.Token.Class', rather
   --  than 'in OpenToken.Token.Handle', in order to accept any
   --  derived type Handle (Ada is annoying in this case!). However,
   --  they are immediately converted to OpenToken.Token.Handle in the
   --  body, so they must have library accessibility level.
   ------------------------------------------------------------------------
   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance;

   ---------------------------------------------------------------------
   --  Create a token sequence from a sequence and a token handle. The
   --  token is added to the sequence.
   ---------------------------------------------------------------------
   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance;
   function "&"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance;

   -------------------------------------------------------------------
   --  Create a token sequence from a pair of sequences. The sequences
   --  are combined to return a single sequence.
   -------------------------------------------------------------------
   function "&"
     (Left  : in Instance;
      Right : in Instance)
     return Instance;

   ----------------------------------------------------------------------
   --  Add a Build action to the instance
   ----------------------------------------------------------------------
   function "+" (Sequence : in Instance; Build : in Action) return Handle;

   ----------------------------------------------------------------------
   --  Set the name
   ----------------------------------------------------------------------
   function "and" (Sequence : in Handle; Name : in String) return Handle;

   ----------------------------------------------------------------------------
   --  Return a newly allocated instance which is a copy of the given
   --  instance, with an optional new name and lookahead.
   ----------------------------------------------------------------------------
   function New_Instance
     (Old_Instance : in Instance;
      Name         : in String   := "";
      Lookahead    : in Integer  := Default_Lookahead;
      Build        : in Action   := null)
     return Handle;

   --------------------------------------------------------------------
   --  Allow dereferencing an expression that returns Handle; needed
   --  when resolving recursion. Just returns Token.
   --
   --  For example : L.all := Copy (A & B + Build'Access).all;
   --------------------------------------------------------------------
   function Copy (Token : in Handle) return Handle;

   procedure Set_Lookahead (Token : in out Instance; Lookahead : in Integer);

   ----------------------------------------------------------------------
   --  When called with Actively => False, the number of tokens to
   --  lookahead is given by Match.Lookahead, which defaults to
   --  Token.Default_Lookahead, but may be overridden by Set_Lookahead.
   ----------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True);

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance);

private
   type Instance is new Parent_Token with record
      Lookahead : Integer;
      Members   : Token.Linked_List.Instance;
      Build     : Action;
   end record;

end OpenToken.Token.Sequence_Mixin;
