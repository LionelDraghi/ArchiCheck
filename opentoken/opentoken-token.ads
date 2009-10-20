-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2000 Ted Dennison
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

-----------------------------------------------------------------------------
--  This package defines an abstract token type, for use by this
--  facility. It also defines a parse operation and a token source
--  type, for use by recursive decent parsers.
-----------------------------------------------------------------------------
package OpenToken.Token is

   type Instance is abstract tagged private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   type Source is abstract tagged private;

   subtype Source_Class is Source'Class;

   type Source_Handle is access all Source_Class;

   --------------------------------------------------------------------------
   --  Abstract specification for a token parse routine.
   --  Implementations of this routine are to verify that current
   --  contents of the input contain this token, and return the value
   --  of the token.
   --
   --  The current token loaded in the Analyzer should be the first
   --  token to parse against. Upon successful completion, the current
   --  token in the analyzer will be the next token after the parsed
   --  token.
   --
   --  An active parse consumes the input, where a non active parse
   --  does not.
   --------------------------------------------------------------------------
   procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     ) is abstract;

   --------------------------------------------------------------------------
   --  This routine should be a quick routine to verify that the given
   --  token can possibly succesfully parse. This routine is meant to
   --  be used for choosing between parsing options, so it should be a
   --  *very* quick check rather than a full parse. In most cases,
   --  simply checking against the analyzer's current token should be
   --  sufficient. But in extreme cases, a call to Parse with Actively
   --  set to False may be required.
   --------------------------------------------------------------------------
   function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class
     ) return Boolean is abstract;

   --------------------------------------------------------------------------
   --  Locate the next token.
   --
   --  If Look_Ahead is set, the next token after the current one will
   --  be returned, but the current one will not be discarded.
   --  Subsequent Look_Ahead calls will return later and later tokens.
   --------------------------------------------------------------------------
   procedure Find_Next (Analyzer   : in out Source;
                        Look_Ahead : in     Boolean := False) is abstract;

   ----------------------------------------------------------------------------
   --  Returns the last token that was matched.
   ----------------------------------------------------------------------------
   function Get (Analyzer : in Source) return Class is abstract;

private
   type Instance is abstract tagged null record;
   type Source   is abstract tagged null record;
end OpenToken.Token;
