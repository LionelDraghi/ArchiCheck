-------------------------------------------------------------------------------
--
--  Copyright (C) 2003, 2008 Stephen Leake
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
--  Maintainer: Stephen Leake (stephen.leake@gsfc.nasa.gov)
--
-------------------------------------------------------------------------------

package body OpenToken.Token.Enumerated.String_Literal is

   function Get (ID     : in Token_ID;
                 Value  : in String := "") return Instance'Class is
   begin
      return Instance'Class (Instance'(ID => ID, Value => Buffers.To_Bounded_String (Value)));
   end Get;

   overriding procedure Create
     (Lexeme     : in     String;
      ID         : in     Token_ID;
      Recognizer : in     Recognizer_Handle;
      New_Token  :    out Instance)
   is
      pragma Unreferenced (Recognizer);
   begin
      New_Token.ID    := ID;
      New_Token.Value := Buffers.To_Bounded_String (Lexeme);
   end Create;

   function Value (Subject : in Instance) return String
   is
      use Buffers;
      Value_Next  : Natural := 1;
      Result      : String (1 .. Length (Subject.Value));
      Result_Last : Natural := Result'First - 1;
      C           : Character;
   begin
      loop
         exit when Value_Next > Length (Subject.Value);
         C := Element (Subject.Value, Value_Next);
         if C = '"' then
            --  First " starts string, last " ends it.
            --  Doubled "" before last is an embedded "
            exit when Value_Next = Length (Subject.Value);
            Value_Next := Value_Next + 1;
            C := Element (Subject.Value, Value_Next);
            --  This might be the last "
            exit when C = '"' and Value_Next = Length (Subject.Value);
         end if;
         Result_Last          := Result_Last + 1;
         Result (Result_Last) := C;
         Value_Next           := Value_Next + 1;
      end loop;

      return Result (1 .. Result_Last);
   end Value;

end OpenToken.Token.Enumerated.String_Literal;
