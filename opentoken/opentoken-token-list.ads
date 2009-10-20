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

-----------------------------------------------------------------------------
--  This package defines a generic list token. A list is a token that
--  is made up of any number of repetitions of other tokens, separated
--  by a given separator token.
-----------------------------------------------------------------------------
package OpenToken.Token.List is

   type Instance is new Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Construct a new list token, using the given Element and Separator tokens.
   ----------------------------------------------------------------------------
   function Get
     (Element   : access OpenToken.Token.Class;
      Separator : access OpenToken.Token.Class)
     return Class;

   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean;

private

   type Instance is new Token.Instance with record
      Element   : OpenToken.Token.Handle;
      Separator : OpenToken.Token.Handle;
   end record;

   ----------------------------------------------------------------------------
   --  This routine is called every time a list element is actively parsed.
   ----------------------------------------------------------------------------
   procedure Add_List_Element
     (Match   : in out Instance;
      Element : in out OpenToken.Token.Class
     ) is null;

   ----------------------------------------------------------------------------
   --  This routine is called when an entire list has been actively parsed.
   ----------------------------------------------------------------------------
   procedure Build (Match : in out Instance) is null;

end OpenToken.Token.List;
