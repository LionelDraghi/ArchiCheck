-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: opentoken-text_feeder-string.adb,v $
-- Revision 1.2  2000/02/05 04:00:21  Ted
-- Added End_Of_Text to support analyzing binaries.
--
-- Revision 1.1  2000/01/27 20:53:25  Ted
-- A settable string text feeder.
--
--
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- This package provides a text feeder class that returns user-defined strings.
-------------------------------------------------------------------------------
package body OpenToken.Text_Feeder.String is

   ----------------------------------------------------------------------------
   -- This function returns strings for the analyzer. This version of it
   -- returns the string given it with the set command the first time it is
   -- called. On subsequent calls, Token.EOF_Character is retured.
   ----------------------------------------------------------------------------
   procedure Get (Feeder   : in out Instance;
                  New_Text :    out Standard.String;
                  Text_End :    out Integer) is
      Data_Length : constant Natural := Ada.Strings.Unbounded.Length (Feeder.Next_Value);
   begin
      if New_Text'Length < Data_Length then
         New_Text := Ada.Strings.Unbounded.To_String
           (Ada.Strings.Unbounded.Head (Source => Feeder.Next_Value,
                                        Count  => New_Text'Length));
         Ada.Strings.Unbounded.Tail (Source => Feeder.Next_Value,
                                     Count  => Data_Length - New_Text'Length);
         Text_End := New_Text'Last;
      else
         Text_End := New_Text'First + Data_Length - 1;
         New_Text(New_Text'First..Text_End) :=
           Ada.Strings.Unbounded.To_String (Feeder.Next_Value);
         Feeder.Next_Value := Ada.Strings.Unbounded.To_Unbounded_String ((1 => OpenToken.Eof_Character));
      end if;
   end Get;

   ----------------------------------------------------------------------------
   -- This function sets the string to be returned the next time Get is called
   ----------------------------------------------------------------------------
   procedure Set (Feeder : out Instance;
                  Value  : in  Standard.String
                 ) is
   begin
      Feeder.Next_Value := Ada.Strings.Unbounded.To_Unbounded_String
        (Value & OpenToken.Eof_Character);
   end Set;

   ----------------------------------------------------------------------------
   -- Return True if there is no more text to process.
   ----------------------------------------------------------------------------
   function End_Of_Text (Feeder : Instance) return Boolean is
   begin
      return Ada.Strings.Unbounded.Length(Feeder.Next_Value) <= 1;
   end End_Of_Text;
end OpenToken.Text_Feeder.String;

