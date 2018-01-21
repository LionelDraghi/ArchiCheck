--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Text_IO;
package body OpenToken.Text_Feeder.Counted_GNAT_OS_Lib is

   function Create (File : in GNAT.OS_Lib.File_Descriptor) return Text_Feeder_Ptr
   is begin
      return new Instance'(File, 0, 0, 0);
   end Create;

   procedure Reset (Feeder : in out Instance; Max_Bytes : in Integer) is
   begin
      Feeder.Max_Bytes  := Max_Bytes;
      Feeder.Read_Bytes := 0;
      Feeder.Get_Count  := 0;
   end Reset;

   overriding procedure Get
     (Feeder   : in out Instance;
      Text     :    out String;
      Text_End :    out Integer)
   is
      use GNAT.OS_Lib;
      Bytes_To_Read : constant Integer := Integer'Min (Text'Length, Feeder.Max_Bytes - Feeder.Read_Bytes);
      Read_Bytes    : Integer;
   begin
      if Feeder.Read_Bytes >= Feeder.Max_Bytes then

         Text_End := Text'First;
         Text (Text_End) := EOF_Character;
      else
         Feeder.Get_Count := Feeder.Get_Count + 1;

         Read_Bytes        := Read (Feeder.File, Text'Address, Bytes_To_Read);
         Feeder.Read_Bytes := Feeder.Read_Bytes + Read_Bytes;

         if Trace_Parse > 0 and Read_Bytes > 0 then
            Ada.Text_IO.Put_Line ("read" & Integer'Image (Read_Bytes));
            Ada.Text_IO.Put_Line (Text (Text'First .. Text'First + Read_Bytes - 1));
         end if;

         Text_End := Text'First + Read_Bytes - 1;

         --  Translate end of line to EOL_Character.
         --  FIXME: what if a line end sequence crosses Text'last?
         for I in Text'First .. Text_End loop
            if I < Text_End and then (Text (I) = ASCII.LF and Text (I + 1) = ASCII.CR) then
               --  DOS line end

               --  We should delete the second character entirely, but
               --  this is simpler. Unless the user code is trying to
               --  reproduce the source exactly, it should be
               --  harmless. We put the space at the end of the prev
               --  line to preserve column numbers in the next.
               Text (I)     := ' ';
               Text (I + 1) := EOL_Character;

            elsif I < Text_End and then (Text (I) = ASCII.CR and Text (I + 1) = ASCII.LF) then
               --  (Old) Mac line end
               Text (I) := ' ';
               Text (I + 1) := EOL_Character;

            elsif Text (I) = ASCII.LF then
               --  Unix line end
               Text (I) := EOL_Character;
            end if;
         end loop;

         if Read_Bytes < Bytes_To_Read then
            --  premature end of file
            Feeder.Read_Bytes := Feeder.Max_Bytes;

            Text_End := Text_End + 1;
            Text (Text_End) := EOF_Character;
         end if;

      end if;
   end Get;

   overriding function End_Of_Text (Feeder : in Instance) return Boolean is
   begin
      return Feeder.Read_Bytes = Feeder.Max_Bytes;
   end End_Of_Text;

   procedure Discard_Rest_Of_Input (Feeder : in out Instance)
   is
      use GNAT.OS_Lib;
      use Ada.Text_IO;
      Start_Bytes : constant Integer := Feeder.Read_Bytes;
      First       : Boolean          := True;
      Junk        : String (1 .. 2048);
      Read_Bytes  : Integer;
      Bytes_To_Read : Integer;
   begin
      if Trace_Parse > 0 then
         New_Line;
         Put_Line ("discarding text from" & Integer'Image (Feeder.Read_Bytes) & ":");
      end if;
      loop
         Bytes_To_Read     := Integer'Min (Junk'Length, Feeder.Max_Bytes - Feeder.Read_Bytes);
         Read_Bytes        := Read (Feeder.File, Junk'Address, Bytes_To_Read);
         Feeder.Read_Bytes := Feeder.Read_Bytes + Read_Bytes;

         if Read_Bytes > 0 and Trace_Parse > 0 and First then
            First := False;
            Put_Line (Junk (1 .. Integer'Min (80, Read_Bytes)));
         end if;
         exit when Read_Bytes < Bytes_To_Read or Feeder.Read_Bytes = Feeder.Max_Bytes;
      end loop;

      if Trace_Parse > 0 then
         Put_Line ("...");
         if Read_Bytes > 80 then
            Put_Line (Junk (Read_Bytes - 80 .. Read_Bytes));
         elsif Read_Bytes > 0 then
            Put_Line (Junk (1 .. Read_Bytes));
         end if;
         Put_Line ("discarded" & Integer'Image (Feeder.Read_Bytes - Start_Bytes));
         Put_Line ("total read" & Integer'Image (Feeder.Read_Bytes) & " of" & Integer'Image (Feeder.Max_Bytes));
      end if;

      Feeder.Read_Bytes := Feeder.Max_Bytes;
   end Discard_Rest_Of_Input;

   function Get_Count (Feeder : in Instance) return Integer
   is begin
      return Feeder.Get_Count;
   end Get_Count;

end OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
