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

-------------------------------------------------------------------------------
--  This package defines a generic list token for recursive descent
--  parsers. A list is a token that is made up of any number of
--  repetitions of other tokens, separated by a given separator token.
-------------------------------------------------------------------------------

generic
   type Parent_Token is abstract new OpenToken.Token.Instance with private;
   type Component_Token is abstract new OpenToken.Token.Instance with private;
package OpenToken.Token.List_Mixin is

   type Instance is new Parent_Token with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding
   function Image (Item : in Instance) return String;

   type List_Action is access procedure (List : in out Instance);

   type Element_Action is access procedure
     (List    : in out Instance;
      Element : in     Component_Token'Class);

   ----------------------------------------------------------------------------
   --  Return a new list token
   ----------------------------------------------------------------------------
   function "**"
     (Element   : access Component_Token'Class;
      Separator : access OpenToken.Token.Class)
     return Instance;

   ----------------------------------------------------------------------
   --  Add an Add_Element action to the instance
   ----------------------------------------------------------------------
   function "*"
     (Token       : in Instance;
      Add_Element : in Element_Action)
     return Handle;

   ----------------------------------------------------------------------
   --  Add an Initialize action to the instance
   ----------------------------------------------------------------------
   function "+"
     (Token      : in Handle;
      Initialize : in List_Action)
     return Handle;

   ----------------------------------------------------------------------
   --  Add a Build action to the instance
   ----------------------------------------------------------------------
   function "-"
     (Token : in Handle;
      Build : in List_Action)
     return Handle;

   function Get
     (Element     : access Component_Token'Class;
      Separator   : access OpenToken.Token.Class;
      Name        : in     String                := "";
      Lookahead   : in     Integer               := Default_Lookahead;
      Initialize  : in     List_Action           := null;
      Add_Element : in     Element_Action        := null;
      Build       : in     List_Action           := null)
     return Instance;

   function New_Instance
     (Token       : in Instance;
      Name        : in String         := "";
      Lookahead   : in Integer        := Default_Lookahead;
      Initialize  : in List_Action    := null;
      Add_Element : in Element_Action := null;
      Build       : in List_Action    := null)
     return Handle;

   procedure Set_Lookahead (Token : in out Instance; Lookahead : in Integer);

   --------------------------------------------------------------------------
   --  Initialize is called before anything else happens.
   --
   --  Add_Element is called with every successive list element
   --  that is recognized.
   --
   --  Build is called when the entire list has been recognized.
   --------------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean      := True);

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance);

private
   type Component_Handle is access all Component_Token'Class;

   type Instance is new Parent_Token with record
      Element     : Component_Handle;
      Separator   : OpenToken.Token.Handle;
      Lookahead   : Integer;
      Initialize  : List_Action;
      Add_Element : Element_Action;
      Build       : List_Action;
   end record;

end OpenToken.Token.List_Mixin;
