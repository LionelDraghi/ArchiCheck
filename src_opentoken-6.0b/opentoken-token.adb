-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014 Stephe Leake
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

with Ada.Tags;
with OpenToken.Token.Linked_List;
package body OpenToken.Token is

   procedure Set_Name (Token : in out Class; Name : in String)
   is begin
      if Name /= "" then
         Token.Name := new String'(Name);
      end if;
   end Set_Name;

   function Name (Token : in Instance) return String is
   begin
      if Token.Name = null then
         return Ada.Tags.External_Tag (Class (Token)'Tag);
      else
         return Token.Name.all;
      end if;
   end Name;

   function Has_Name (Token : in Instance) return Boolean
   is begin
      return Token.Name /= null;
   end Has_Name;

   function Name_Dispatch (Token : in Class) return String
   is begin
      return Name (Token);
   end Name_Dispatch;

   function Name_Dispatch (Token : access constant Instance'Class) return String
   is begin
      return Name (Token.all);
   end Name_Dispatch;

   procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is begin
      Linked_List.Add (List, Handle (Token));
   end Expecting;

end OpenToken.Token;
