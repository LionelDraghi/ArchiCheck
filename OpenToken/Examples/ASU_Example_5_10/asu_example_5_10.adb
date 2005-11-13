-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-- $Log: asu_example_5_10.adb,v $
-- Revision 1.3  2000/08/07 00:25:58  Ted
-- moved to -run.adb
--
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

-------------------------------------------------------------------------------
-- This example is an implementation of Example 5.10 from "Compilers
-- Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
-- Dragon Book"). It demonstrates handling of synthesized attributes
-------------------------------------------------------------------------------
package body ASU_Example_5_10 is

   --------------------------------------------------------------------------
   -- Routine to print the value of the given Single integer token.
   --------------------------------------------------------------------------
   procedure Print_Value (New_Token : out Nonterminal.Class;
                          Source    : in  Token_List.Instance'Class;
                          To_ID     : in  Master_Token.Token_ID) is

   begin
      Ada.Text_IO.Put_Line
        (Integer'Image
         (Simple_Integer.Value
          (Simple_Integer.Class
           (Token_List.Token_Handle
            (Token_List.Initial_Iterator(Source)
             ).all
            )
           )
          )
         );

      Nonterminal.Synthesize_Self (New_Token => New_Token,
                                   Source    => Source,
                                   To_ID     => To_ID);
   end Print_Value;

end ASU_Example_5_10;

