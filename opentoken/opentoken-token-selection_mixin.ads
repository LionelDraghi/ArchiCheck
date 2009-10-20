-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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

with OpenToken.Token.Linked_List;

-------------------------------------------------------------------------------
--  This package defines a reusable token for a simple selection between tokens.
--  These a quite easy to create yourself, of course. But having a prebuilt one
--  allows you to easily use it in constructors for other tokens.
-------------------------------------------------------------------------------
generic
   type Parent_Token is abstract new OpenToken.Token.Instance with private;
   type Component_Token is abstract new OpenToken.Token.Instance with private;

package OpenToken.Token.Selection_Mixin is

   type Instance is new Parent_Token with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Retrieve the given selection token from the analyzer.
   --  The default implementaition chooses which token to parse by calling
   --  Could_Parse_To on every token in the selection until one returns true.
   --  If the token is not LL(1), that is, if it needs to examine more than one
   --  token from the input stream to determine which selection to make, then
   --  this behavior won't parse correctly. This is a problem because all the
   --  supplied Parse and Could_Parse_To routines only check the first token.
   --  To fix this, you have a several options. You can:
   --    o  Implement your own token type to do the parsing. You can look ahead
   --       multiple tokens by setting the Actively flag on the parse routine.
   --    o  Derive a type from Instance, and provide your own Parse routine.
   --       This routine should examine enough tokens ahead (using the Actively
   --       flag where it needs to) in order to make the proper decision.
   --    o  Override the default implementations of Could_Parse_To for the
   --       tokens in the selection (not the selection token itself). The
   --       overridden routines can check multiple tokens ahead instead of
   --       just one.
   --    o  Rearrange your token defintions so that only one token of
   --       lookahead is required.
   --  The private routine Build is called when the entire operation
   --  has been recognized.
   --  An a non active parse does not comsume any input from the analyzer,
   --  and does not call any of the private routines.
   ----------------------------------------------------------------------------
   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Create a token selection from a pair of token instances.
   ----------------------------------------------------------------------------
   function "or" (Left  : access Component_Token'Class;
                  Right : access Component_Token'Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token selection from a token handle and a token selection.
   ----------------------------------------------------------------------------
   function "or" (Left  : access Component_Token'Class;
                  Right : in     Instance) return Instance;
   function "or" (Left  : in     Instance;
                  Right : access Component_Token'Class) return Instance;

   ----------------------------------------------------------------------------
   --  Create a token selection from a pair of selection tokens
   ----------------------------------------------------------------------------
   function "or" (Left  : in Instance;
                  Right : in Instance) return Instance;


   ----------------------------------------------------------------------------
   --  This routine should is a quick check to verify that the given operation
   --  token can possibly succesfully parse from what's sitting in the analyzer.
   --  This routine is meant to be used for choosing between parsing options.
   --  It simply checks Could_Parse_To for this token's Element token.
   --  The default implementation calls Could_Parse_To for every token in the
   --  selection.
   ----------------------------------------------------------------------------
   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean;

   ----------------------------------------------------------------------------
   --  This routine is called when none of the sequence's tokens return true for
   --  Could_Parse_To. It raises parse error. If Actively is set, it includes
   --  a descriptive exception message in the exception.
   --  The default version of this routine tries to identify the possible tokens
   --  by their tag name. This is a pretty lame scheme if reusable tokens are
   --  used, so you will probably either want to override this, or handle the
   --  parse error in the calling routine and print out a better message there.
   ----------------------------------------------------------------------------
   procedure Raise_Parse_Error
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     );

   ----------------------------------------------------------------------------
   --  This routine is called when an entire selection has been actively
   --  parsed. The second parameter contains the token that was parsed. An
   --  implementation of this routine could then check the 'tag or ID of From to
   --  figure out which selection was matched.
   --  The default implementation does nothing.
   ----------------------------------------------------------------------------
   procedure Build (Match : in out Instance;
                    From  : in     Component_Token'Class) is null;

private

   type Instance is new Parent_Token with record
      Members : Token.Linked_List.Instance;
   end record;

end OpenToken.Token.Selection_Mixin;
