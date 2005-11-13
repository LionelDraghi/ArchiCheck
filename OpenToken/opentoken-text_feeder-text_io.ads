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
-- $Log: opentoken-text_feeder-text_io.ads,v $
-- Revision 1.2  2000/02/05 04:00:21  Ted
-- Added End_Of_Text to support analyzing binaries.
--
-- Revision 1.1  2000/01/27 20:54:44  Ted
-- A text feeder based on Text_IO files.
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;

-------------------------------------------------------------------------------
-- This package provides an implementation of a text feeder class that relies
-- on Ada.Text_IO for the underlying mechanism.
-- Each text line is terminated with an OpenToken.EOF_Character, no matter
-- what actual character (if any) the OS uses for that purpose. This makes
-- this package fairly portable. Unfortunately, it also inherits Text_IO's
-- problem of not being able to read text on the same line as the end of
-- the file. That means all text files used with this feeder need to have
-- a line termintor on the last line of text, or it will be ignored.
-------------------------------------------------------------------------------
package OpenToken.Text_Feeder.Text_IO is


   -- An instance of a Text_IO feeder. By default, it will read from Text_IO's
   -- current input file.
   type Instance is new OpenToken.Text_Feeder.Instance with private;

   ----------------------------------------------------------------------------
   -- Create a Text Feeder for the given Text_IO file.
   ----------------------------------------------------------------------------
   function Create (File_Ptr : Ada.Text_IO.File_Access := Ada.Text_IO.Current_Input)
     return Instance;

   ----------------------------------------------------------------------------
   -- This procedure returns strings for the analyzer. If the end of the
   -- file is reached, a Token.EOF_Character is retured.
   -- Each text line is terminated with an OpenToken.EOF_Character, no matter
   -- what actual character (if any) the OS uses for that purpose.
   ----------------------------------------------------------------------------
   procedure Get (Feeder   : in out Instance;
                  New_Text :    out String;
                  Text_End :    out Integer);

   ----------------------------------------------------------------------------
   -- Return True if there is no more text to process. For this feeder, that
   -- happens when we are at the end of the file.
   ----------------------------------------------------------------------------
   function End_Of_Text (Feeder : Instance) return Boolean;

private
   Nothing : constant Character := Character'Val(0);

   type Instance is new OpenToken.Text_Feeder.Instance with record
      File     : Ada.Text_IO.File_Access := Ada.Text_IO.Current_Input;
      Ended    : Boolean := False;
   end record;

end OpenToken.Text_Feeder.Text_IO;

