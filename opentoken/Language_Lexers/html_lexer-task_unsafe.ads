-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephen Leake
-- Copyright (C) 1999, 2000 Christoph Karl Walter Grein
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
-------------------------------------------------------------------------------

with OpenToken.Text_Feeder.Text_IO;
package HTML_Lexer.Task_Unsafe is

   ------------------------------------------------------------------------
   --  This is a lexical analyser for the HTML language.
   --
   --  It uses package level variables in the body, so it is not task
   --  safe.
   --
   --   <Tag Attribute=Value Attribute="String"> Text with &entity; </Tag>
   ------------------------------------------------------------------------

   procedure Initialize (Input_Feeder : in OpenToken.Text_Feeder.Text_IO.Instance);

   function Next_Token return HTML_Token;

end HTML_Lexer.Task_Unsafe;
