-------------------------------------------------------------------------------
--
--  Copyright (C) 2003, 2008, 2009, 2014 Stephen Leake
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
-------------------------------------------------------------------------------

package body OpenToken.Token.Enumerated.String is

   --------------------------------------------------------------------------
   --  Return the value of Item with quotes removed
   --  (assumes Ada syntax).
   --------------------------------------------------------------------------
   function Unquote (Item : in Standard.String) return Standard.String
   is
      use Buffers;
      Item_Next  : Natural := Item'First;
      Result      : Standard.String (1 .. Item'Length);
      Result_Last : Natural := Result'First - 1;
      C           : Character;
   begin
      loop
         exit when Item_Next > Item'Last;
         C := Item (Item_Next);
         if C = '"' then
            --  First " starts string, last " ends it.
            --  Doubled "" before last is an embedded "
            exit when Item_Next = Item'Last;
            Item_Next := Item_Next + 1;
            C := Item (Item_Next);
            --  This might be the last "
            exit when C = '"' and Item_Next = Item'Last;
         end if;
         Result_Last          := Result_Last + 1;
         Result (Result_Last) := C;
         Item_Next            := Item_Next + 1;
      end loop;

      return Result (1 .. Result_Last);
   end Unquote;

   function Get
     (ID    : in Token_ID;
      Value : in Standard.String := "";
      Name  : in Standard.String := "";
      Build : in Action          := null)
     return Instance'Class
   is begin
      if Name = "" then
         return Instance'Class (Instance'(null, ID, Build, Buffers.To_Bounded_String (Value)));
      else
         return Instance'Class (Instance'(new Standard.String'(Name), ID, Build, Buffers.To_Bounded_String (Value)));
      end if;
   end Get;

   overriding procedure Create
     (Lexeme     : in     Standard.String;
      Bounds     : in     Buffer_Range;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance)
   is
      pragma Unreferenced (Bounds);
      pragma Unreferenced (Recognizer);
   begin
      New_Token.Value := Buffers.To_Bounded_String (Unquote (Lexeme));
   end Create;

   overriding procedure Copy
     (To   : in out Instance;
      From : in     Token.Class)
   is begin
      To.Value := Instance (From).Value;
   end Copy;

end OpenToken.Token.Enumerated.String;
