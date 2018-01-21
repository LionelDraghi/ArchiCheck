--  Abstract :
--
--  Identifier token
--
--  Copyright (C) 2002, 2009, 2014 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

generic
package OpenToken.Token.Enumerated.Identifier is

   type Instance is new Enumerated.Instance with record
      --  Because OpenToken.Token.Enumerator.Analyzer reuses token
      --  objects, we can't use a discriminant to set the string
      --  length. So we use Ada.Strings.Bounded.
      Identifier : OpenToken.Buffers.Bounded_String;
   end record;

   function Get
     (ID    : in Token_ID;
      Build : in Action := null)
     return Instance'Class;

   overriding procedure Create
     (Lexeme     : in     String;
      Bounds     : in     Buffer_Range;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance);

   overriding procedure Copy
     (To   : in out Instance;
      From : in     Token.Class);

end OpenToken.Token.Enumerated.Identifier;
