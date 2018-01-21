-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2012, 2014 Stephe Leake
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

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.List;

-----------------------------------------------------------------------------
--  This package provides a type and operations for building grammar
--  productions.
-----------------------------------------------------------------------------

generic
   with package Token_List is new OpenToken.Token.Enumerated.List;
package OpenToken.Token.Enumerated.Nonterminal is

   type Instance is new OpenToken.Token.Enumerated.Instance with null record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Get a nonterminal token with the given ID.
   --
   --  Return type is 'class to avoid forcing derived types to override this
   ----------------------------------------------------------------------------
   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class;

   --  Return a newly allocated copy of Token, or null
   function Copy (Token : in Handle) return Handle;

   --------------------------------------------------------------------------
   --  The following primitive routines provide overloadable
   --  interfaces for synthesizing a nonterminal. Child types can
   --  provide more of these. Each will need a function of type
   --  Synthesize which calls it.

   --  This exception should get raised when token argument for
   --  synthesization is of the wrong type.
   Invalid_Synth_Argument : exception;

   --------------------------------------------------------------------------
   --  Synthesization routines for use in creating productions. These
   --  routines are called automaticly by the parser when a production
   --  is recognized in the input (aka: "reduced").

   --------------------------------------------------------------------------
   --  Synthesize a non-terminal instance from a given list of
   --  terminals. Routines of this type can be called by the parser
   --  when it detects a series of tokens that derive to this token.
   --------------------------------------------------------------------------
   type Synthesize is access procedure (New_Token : out Class;
                                        Source    : in  Token_List.Instance'Class;
                                        To_ID     : in  Token_ID);

   --------------------------------------------------------------------------
   --  Default synthesization routine. If no synthesization routine is
   --  specified for the production, this routine will be called. It
   --  dispatches to the New_Token instance type's Default_Synthesize
   --  routine. For this instance, that has the same effect as calling
   --  Synthesize_First.all. See the header for that routine for more
   --  information.
   --
   --  To change the default behavior in a new token derived from
   --  Instance, override the Default_Synthesize routine.
   --
   --  See the comment on the left-hand-side token's
   --  Default_Synthesize primitive for the list of restrictions for
   --  what tokens may legally appear on the right hand side. If these
   --  restrictions are violated, any exception may be raised, but
   --  Invalid_Synth_Argument and Constraint_Error are most likely.
   --------------------------------------------------------------------------
   Synthesize_Default : constant Synthesize;

   --------------------------------------------------------------------------
   --  Optional synthesization routine. It Creates a new version of a
   --  Nonterminal token, ex nihilo. It just fills in the ID using the
   --  To_ID and returns the result. For this package, this should be
   --  the same result, but much faster than, Synthesize_First. But
   --  since this isn't in general possible for more complex tokens,
   --  it wasn't made inheritable. Thus this routine can only produce
   --  Nonterminal.Instance's.
   --
   --  The tokens on the right hand side of the production are ignored.
   ----------------------------------------------------------------------------
   Synthesize_Self  : constant Synthesize;

   --------------------------------------------------------------------------
   --  Optional synthesization routine. Passes the first token in the
   --  list to the instance's Synthesize_By_Copying routine, which by
   --  default will up-convert it into the proper nonterminal instance
   --  type, set the ID to the given value, and return it.
   --
   --  If the first token in the source list is not a nonterminal in
   --  the Instance'Class of token on the left-hand side of the
   --  production, Invalid_Synth_Argument will be raised. Also, you
   --  should see the comment on the left-hand-side token's primitive
   --  for the list of restrictions for what tokens may legally appear
   --  in the first position on the production's right hand side. If
   --  these restrictions are violated, any exception may be raised,
   --  but Invalid_Synth_Argument and Constraint_Error are most
   --  likely.
   --------------------------------------------------------------------------
   Synthesize_First : constant Synthesize;

   --------------------------------------------------------------------------
   --  Dispatching versions of Token synthesization routines. These
   --  routines may be overridden for types derived from Instance to
   --  provide custom behavior for the visible synthesization
   --  procedure pointers.
   --
   --  These routines aren't intended to be be used directly. They are
   --  made public simply so that non-child packages with types
   --  derived from Instance can override them.

   --------------------------------------------------------------------------
   --  Create a token by simply up-converting the given token, and
   --  changing its ID to match the given ID. For this default
   --  implementation, the Source must be in Instance'Class.
   --------------------------------------------------------------------------
   procedure Synthesize_By_Copying (New_Token : out Instance;
                                    Source    : in  OpenToken.Token.Enumerated.Instance'Class;
                                    To_ID     : in  Token_ID);


   --------------------------------------------------------------------------
   --  The default attribute synthesization routine. If no
   --  synthesization routine is specified by users when they make a
   --  production, this routine will be dispatched to. The default
   --  implementation provided in this package has the same effect and
   --  restrictions as Synthesize_First.
   --------------------------------------------------------------------------
   procedure Default_Synthesize (New_Token : out Instance;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  Token_ID);

private

   --------------------------------------------------------------------------
   --  Optional synthesization routine. It Creates a new version of a
   --  Nonterminal token, ex nihilo. It just fills in the ID using the
   --  To_ID and returns the result. For this package, this should be
   --  the same result, but much faster than, Synthesize_First. But
   --  since this isn't in general possible for more complex tokens,
   --  it wasn't made inheritable. Thus this routine can only produce
   --  Nonterminal.Instance's.
   --------------------------------------------------------------------------
   procedure Self_Synthesize (New_Token : out Class;
                              Source    : in  Token_List.Instance'Class;
                              To_ID     : in  Token_ID);

   --------------------------------------------------------------------------
   --  Optional synthesization routine. Passes the first token in the
   --  list to the instance's Synthesize_By_Copying routine, which by
   --  default will up-convert it into the proper nonterminal instance
   --  type, set the ID to the given value, and return it. If the
   --  first token in the source list is not a nonterminal in the
   --  return production target's Instance'Class, Constraint_Error
   --  will be raised.
   --------------------------------------------------------------------------
   procedure Synthesize_From_First (New_Token : out Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Token_ID);

   --------------------------------------------------------------------------
   --  Default synthesization routine. This routine dispatches to the
   --  return instance type's Default_Synthesize routine. To change
   --  the default synthesization behavior, override the
   --  Default_Synthesize routine.
   --------------------------------------------------------------------------
   procedure Default_Synthesize_Class (New_Token : out Class;
                                       Source    : in  Token_List.Instance'Class;
                                       To_ID     : in  Token_ID);

   Synthesize_Self    : constant Synthesize := Self_Synthesize'Access;
   Synthesize_First   : constant Synthesize := Synthesize_From_First'Access;
   Synthesize_Default : constant Synthesize := Default_Synthesize_Class'Access;
end OpenToken.Token.Enumerated.Nonterminal;
