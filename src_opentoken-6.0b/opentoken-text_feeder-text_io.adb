-------------------------------------------------------------------------------
--
-- Copyright (C) 2012 Stephen Leake
-- Copyright (C) 1999 Ted Dennison
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

package body OpenToken.Text_Feeder.Text_IO is

   function Create (File_Ptr : Ada.Text_IO.File_Access) return Instance
   is begin
      return (File_Ptr, False);
   end Create;

   function Create (File_Ptr : in Ada.Text_IO.File_Access) return Text_Feeder_Ptr
   is begin
      return new Instance'(Create (File_Ptr));
   end Create;

   overriding procedure Get
     (Feeder   : in out Instance;
      New_Text :    out String;
      Text_End :    out Integer)
   is begin

      --  Get as much text as possible from Text_IO
      Ada.Text_IO.Get_Line (File => Feeder.File.all,
                            Item => New_Text,
                            Last => Text_End);

      Feeder.Ended := False;
      if Text_End < New_Text'Last then
         --  Figure out if it should end with an end of file or end of line
         Text_End := Text_End + 1;
         if Ada.Text_IO.End_Of_File (Feeder.File.all) then
            New_Text (Text_End) := EOF_Character;
            Feeder.Ended := True;
         else
            New_Text (Text_End) := EOL_Character;
         end if;
      end if;
   exception
   when Ada.Text_IO.End_Error =>
      Text_End := New_Text'First;
      New_Text (New_Text'First) := EOF_Character;
      Feeder.Ended := True;
   end Get;

   overriding function End_Of_Text (Feeder : Instance) return Boolean
   is begin
      return Feeder.Ended and then Ada.Text_IO.End_Of_File (Feeder.File.all);
   end End_Of_Text;

end OpenToken.Text_Feeder.Text_IO;
