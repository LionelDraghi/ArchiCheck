--  Abstract :
--
--  Text feed that reads a limited number of bytes from a GNAT_OS_Lib
--  stream.
--
--  This provides immediate response; on Windows when the actual input
--  for Standard_Input is not a terminal, Ada.Text_IO.Get does not
--  return until some rather large buffer fills up. This does not
--  happen with GNAT_OS_Lib.Read.
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

with GNAT.OS_Lib;
package OpenToken.Text_Feeder.Counted_GNAT_OS_Lib is

   type Instance is new OpenToken.Text_Feeder.Instance with private;

   function Create (File : in GNAT.OS_Lib.File_Descriptor) return Text_Feeder_Ptr;
   --  File must be open.

   procedure Reset (Feeder : in out Instance; Max_Bytes : in Integer);

   overriding procedure Get
     (Feeder   : in out Instance;
      Text     :    out String;
      Text_End :    out Integer);

   overriding function End_Of_Text (Feeder : in Instance) return Boolean;
   --  Returns True after Max_Bytes has been read, or the .

   procedure Discard_Rest_Of_Input (Feeder : in out Instance);

   --  For profiling

   function Get_Count (Feeder : in Instance) return Integer;
   --  Count of calls to Get since Reset before end of text.
private

   type Instance is new OpenToken.Text_Feeder.Instance with record
      File       : GNAT.OS_Lib.File_Descriptor;
      Max_Bytes  : Integer;
      Read_Bytes : Integer;

      Get_Count : Integer;
   end record;

end OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
