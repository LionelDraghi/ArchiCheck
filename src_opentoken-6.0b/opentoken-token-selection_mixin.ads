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
--  This package defines a simple selection token for recursive
--  descent parsers.
-----------------------------------------------------------------------------

with OpenToken.Token.Linked_List;
generic
   type Parent_Token is abstract new OpenToken.Token.Instance with private;
   type Component_Token is abstract new OpenToken.Token.Instance with private;

package OpenToken.Token.Selection_Mixin is

   type Instance is new Parent_Token with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding
   function Image (Item : in Instance) return String;

   type Action is access procedure
     (Match : in out Instance;
      From  : in     Component_Token'Class);

   --------------------------------------------------------------------
   --  Create a token selection from a pair of token instances.
   --
   --  If either is a selection, it is included by reference; the
   --  member list is _not_ examined. Together with returning Instance
   --  rather than Handle, this allows for controlled recursion. It
   --  also requires the use of New_Selection or "+" to return an object
   --  compatible with Sequence and other tokens, which has the effect
   --  of making it clear when recursion is desired.
   --
   --  These arguments must be 'access OpenToken.Token.Class', rather
   --  than 'in OpenToken.Token.Handle', in order to accept any
   --  derived type Handle (Ada is annoying in this case!). However,
   --  they are immediately converted to OpenToken.Token.Handle in the
   --  body, so they must have library accessibility level.
   --
   --  We rename each "or" as "/", because "/" has higher precedence
   --  than "+", while "or" has lower. Thus when specifying a build
   --  action, we can use either:
   --
   --     E := (A or B) + Build'Access;
   --     E := A / B + Build'Access;
   --
   --  "/" is close to "|", the selection operator in Backus-Naur
   --  form, but "or" is closer in English meaning.
   --
   --  GNAT warns about the odd renaming, so we suppress that warning.
   -------------------------------------------------------------------
   function "or"
     (Left  : access Component_Token'Class;
      Right : access Component_Token'Class)
     return Instance;
   pragma Warnings (Off);
   function "/"
     (Left  : access Component_Token'Class;
      Right : access Component_Token'Class)
     return Instance renames "or";
   pragma Warnings (On);

   -------------------------------------------------------------------
   --  Create a token selection from a selection and a token handle.
   --  The token is added to the selection.
   -------------------------------------------------------------------
   function "or"
     (Left  : access Component_Token'Class;
      Right : in     Instance)
     return Instance;
   pragma Warnings (Off);
   function "/"
     (Left  : access Component_Token'Class;
      Right : in     Instance)
     return Instance renames "or";
   pragma Warnings (On);
   function "or"
     (Left  : in     Instance;
      Right : access Component_Token'Class)
     return Instance;
   pragma Warnings (Off);
   function "/"
     (Left  : in     Instance;
      Right : access Component_Token'Class)
     return Instance renames "or";
   pragma Warnings (On);

   -----------------------------------------------------------------
   --  Create a token selection from a pair of selections. The
   --  selections are combined to return a single selection.
   -----------------------------------------------------------------
   function "or"
     (Left  : in Instance;
      Right : in Instance)
     return Instance;
   pragma Warnings (Off);
   function "/"
     (Left  : in Instance;
      Right : in Instance)
     return Instance renames "or";
   pragma Warnings (On);

   ----------------------------------------------------------------------
   --  Add a Build action to the instance
   ----------------------------------------------------------------------
   function "+"
     (Selection : in Instance;
      Build     : in Action)
     return Handle;

   ----------------------------------------------------------------------
   --  Set the name
   ----------------------------------------------------------------------
   function "and" (Selection : in Handle; Name : in String) return Handle;

   ----------------------------------------------------------------------------
   --  Return a newly allocated instance which is a copy of the given
   --  instance, with an optional new name.
   ----------------------------------------------------------------------------
   function New_Instance
     (Old_Instance : in Instance;
      Name         : in String   := "";
      Build        : in Action   := null)
     return Handle;

   --------------------------------------------------------------------
   --  Allow dereferencing an expression that returns Handle; needed
   --  when resolving recursion. Just returns Token.
   --
   --  For example : L.all := Copy (A & B + Build'Access).all;
   --------------------------------------------------------------------
   function Copy (Token : in Handle) return Handle;

   --------------------------------------------------------------------
   --  The Build action specified in New_Instance or "+" is called when
   --  an entire selection has been actively parsed. From is the token
   --  that was selected.
   --------------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean      := True);

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance);

private

   type Instance is new Parent_Token with record
      Members : Token.Linked_List.Instance;
      Build   : Action;
   end record;

end OpenToken.Token.Selection_Mixin;
