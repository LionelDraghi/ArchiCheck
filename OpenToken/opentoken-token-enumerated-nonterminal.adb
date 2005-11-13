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
-- $Log: opentoken-token-enumerated-nonterminal.adb,v $
-- Revision 1.2  2000/08/12 23:58:06  Ted
-- Removed unused variables
--
-- Revision 1.1  2000/08/12 14:07:51  Ted
-- moved from opentoken-token-nonterminal
--
-- Revision 1.1  2000/01/27 20:59:04  Ted
-- Nonterminal tokens (for productions).
--
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;

-------------------------------------------------------------------------------
-- This package provides a type and operatoions for building grammar
-- productions.
-------------------------------------------------------------------------------
package body OpenToken.Token.Enumerated.Nonterminal is

   ----------------------------------------------------------------------------
   -- Get a token with the given ID.
   ----------------------------------------------------------------------------
   function Get (ID : in Token_ID := Token_ID'First) return Instance'Class is
   begin
      return Instance'Class(Instance'(ID => ID));
   end Get;

   ----------------------------------------------------------------------------
   -- Create a token by simply up-converting the given token, and
   -- changing its ID to match the given ID. For this default implementation,
   -- the Source must be in Instance'Class.
   ----------------------------------------------------------------------------
   procedure Synthesize_By_Copying (New_Token : out Instance;
                                    Source    : in  OpenToken.Token.Enumerated.Instance'Class;
                                    To_ID     : in  OpenToken.Token.Enumerated.Token_ID) is

   begin
      New_Token := Instance(Source);
      New_Token.ID := To_ID;

   exception
      when Constraint_Error =>
         Ada.Exceptions.Raise_Exception
           (Invalid_Synth_Argument'Identity,
            "Token " & OpenToken.Token.Enumerated.Token_ID'Image(To_ID) & " cannot be synthesized " &
            "solely from a " &
            OpenToken.Token.Enumerated.Token_ID'Image (OpenToken.Token.Enumerated.ID(Source)) & "."
            );

   end Synthesize_By_Copying;

   ----------------------------------------------------------------------------
   -- The default attribute synthesization routine. If no synthesization
   -- routine is specified by users when they make a production, this routine
   -- will be dispatched to. The default implementation provided in this
   -- package has the same effect as Synthesize_First.
   ----------------------------------------------------------------------------
   procedure Default_Synthesize (New_Token : out Instance;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  OpenToken.Token.Enumerated.Token_ID) is
   begin
      Synthesize_By_Copying
        (New_Token => Class(New_Token),
         Source    => Token_List.Token_Handle(Token_List.Initial_Iterator(Source)).all,
         To_ID     => To_ID
         );
   end Default_Synthesize;

   ----------------------------------------------------------------------------
   -- Optional synthesization routine. It Creates a new version of a
   -- Nonterminal token, ex nihilo. It just fills in the ID using the To_ID
   -- and returns the result.
   -- For this package, this should be the same result, but much faster than,
   -- Synthesize_First. But since this isn't in general possible for more
   -- complex tokens, it wasn't made inheritable. Thus this routine can only
   -- produce Nonterminal.Instance's.
   ----------------------------------------------------------------------------
   procedure Self_Synthesize (New_Token : out Class;
                              Source    : in  Token_List.Instance'Class;
                              To_ID     : in  OpenToken.Token.Enumerated.Token_ID) is
   begin
      New_Token.ID := To_ID;
   end Self_Synthesize;

   ----------------------------------------------------------------------------
   -- Optional synthesization routine. Passes the first token in the list to
   -- the instance's Synthesize_By_Copying routine, which by default will
   -- up-convert it into the proper nonterminal instance type, set the ID to
   -- the given value, and return it.
   -- If the first token in the source list is not a nonterminal in the return
   -- production target's Instance'Class, Constraint_Error will be raised.
   ----------------------------------------------------------------------------
   procedure Synthesize_From_First (New_Token : out Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  OpenToken.Token.Enumerated.Token_ID) is

      Checked_Source : Handle;
   begin
      begin
         Checked_Source := Handle(Token_List.Token_Handle(Token_List.Initial_Iterator(Source)));
      exception
         when Constraint_Error =>
            Ada.Exceptions.Raise_Exception
              (Invalid_Synth_Argument'Identity,
               "Token " & OpenToken.Token.Enumerated.Token_ID'Image(To_ID) & " cannot be synthesized " &
               "solely from a " &
               OpenToken.Token.Enumerated.Token_ID'Image
               (OpenToken.Token.Enumerated.ID
                (Token_List.Token_Handle(Token_List.Initial_Iterator(Source)).all
                 )
                ) & "."
               );
      end;

      Synthesize_By_Copying
        (New_Token => New_Token,
         Source    => Checked_Source.all,
         To_ID     => To_ID
         );
   end Synthesize_From_First;


   ----------------------------------------------------------------------------
   -- Default synthesization routine. This routine dispatches to the return
   -- instance type's Default_Synthesize routine. To change the default
   -- synthesization behavior, override the Default_Synthesize routine.
   ----------------------------------------------------------------------------
   procedure Default_Synthesize_Class (New_Token : out Class;
                                       Source    : in  Token_List.Instance'Class;
                                       To_ID     : in  OpenToken.Token.Enumerated.Token_ID) is
   begin
      Default_Synthesize (New_Token => New_Token,
                          Source    => Source,
                          To_ID     => To_ID);
   end Default_Synthesize_Class;

end OpenToken.Token.Enumerated.Nonterminal;
