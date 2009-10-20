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
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This package defines a generic list token. A list is a token that is made
--  up of any number of repetitions of other tokens, separated by a given
--  separator token.
-------------------------------------------------------------------------------
generic
   type Parent_Token is abstract new OpenToken.Token.Instance with private;
   type Component_Token is abstract new OpenToken.Token.Instance with private;
package OpenToken.Token.List_Mixin is

   type Instance is new Parent_Token with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Retrieve a list token from the analyzer.
   --  The private routine Add_List_Element is called with every successive list
   --  element that is recognized. The private routine Build is called when the
   --  entire list has been recognized.
   --  An a non active parse does not comsume any input from the analyzer,
   --  and does not call any of the private routines.
   ----------------------------------------------------------------------------
   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Construct a new list token, using the given Element and Separator tokens.
   ----------------------------------------------------------------------------
   function Get
     (Element   : access Component_Token'Class;
      Separator : access OpenToken.Token.Class
     ) return Instance;

   ----------------------------------------------------------------------------
   --  This routine should is a quick check to verify that the given list token
   --  can possibly succesfully parse from what's sitting in the analyzer.
   --  This routine is meant to be used for choosing between parsing options.
   --  It simply checks Could_Parse_To for this token's Element token.
   ----------------------------------------------------------------------------
   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean;

   ----------------------------------------------------------------------------
   --  This routine is called when a parse starts to clean up any saved data.
   --  The default implementation does nothing.
   ----------------------------------------------------------------------------
   procedure Initialize (Match : in out Instance) is null;

   ----------------------------------------------------------------------------
   --  This routine is called every time a list element is actively parsed.
   --  The default implementation does nothing.
   ----------------------------------------------------------------------------
   procedure Add_List_Element
     (Match   : in out Instance;
      Element : in out Component_Token'Class) is null;

   ----------------------------------------------------------------------------
   --  This routine is called when an entire list has been actively parsed.
   --  The default implementation does nothing.
   ----------------------------------------------------------------------------
   procedure Build (Match : in out Instance) is null;

private
   type Component_Handle is access all Component_Token'Class;

   type Instance is new Parent_Token with record
      Element   : Component_Handle;
      Separator : OpenToken.Token.Handle;
   end record;

end OpenToken.Token.List_Mixin;
