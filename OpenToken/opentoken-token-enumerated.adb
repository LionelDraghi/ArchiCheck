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
-- $Log: opentoken-token-enumerated.adb,v $
-- Revision 1.1  2000/08/12 14:11:13  Ted
-- moved from opentoken-token
--
-- Revision 1.1  2000/01/27 20:59:37  Ted
-- Added some common routines.
--
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Tags;

-------------------------------------------------------------------------------
-- This package is the top of a generic hierarchy. Based on the list of IDs
-- it is instantiated with, a user can create tokens and token analyzers.
--
-- This package declares an type for designating a single token. It is
-- designed to be created by an instance of the Token.Analyzer class when a
-- particular kind of token is recognized.
-------------------------------------------------------------------------------
package body OpenToken.Token.Enumerated is

   ----------------------------------------------------------------------------
   -- Get a nonterminal token with the given ID.
   ----------------------------------------------------------------------------
   function Get (ID : in Token_ID := Token_ID'First) return Instance'Class is
   begin
      return Instance'Class(Instance'(ID => ID));
   end Get;

   ----------------------------------------------------------------------------
   -- This procedure will be called when a token is recognized.
   --
   -- The Token's ID will be set to the given value. The Lexeme and Recognizer
   -- fields aren't used for this instance of the type. But they will be
   -- filled in by the analyzer.
   ----------------------------------------------------------------------------
   procedure Create (Lexeme     : in     String;
                     ID         : in     Token_ID;
                     Recognizer : in     Recognizer_Handle;
                     New_Token  :    out Instance) is
   begin
      New_Token.ID := ID;
   end Create;

   ----------------------------------------------------------------------------
   -- This function returns the ID of the token.
   ----------------------------------------------------------------------------
   function ID (Token : in Instance'Class) return Token_ID is
   begin
      return Token.ID;
   end ID;

   ----------------------------------------------------------------------------
   -- Set the given token's ID to the given value
   ----------------------------------------------------------------------------
   procedure Set_ID (Token : in out Instance'Class;
                    ID    : in     Token_ID) is
   begin
      Token.ID := ID;
   end Set_ID;

   ----------------------------------------------------------------------------
   -- Implementation for a token parse routine.
   --
   -- The default version of this routine checks that the ID of the next token
   -- matches the Match ID.
   --
   -- An active parse consumes the input, where a non active parse does not.
   ----------------------------------------------------------------------------
   procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     ) is

      Next_Token : constant OpenToken.Token.Class := Get (Analyzer);
   begin
      if Instance(Next_Token).ID = Match.ID then
         Match := Instance(Next_Token);

         if Actively then
            Create
              (Lexeme     => Lexeme(Source'Class(Analyzer)),
               ID         => Match.ID,
               Recognizer => Last_Recognizer(Source'Class(Analyzer)),
               New_Token  => Class(Match)
               );
         end if;
      else
         Ada.Exceptions.Raise_Exception
           (Parse_Error'Identity,
            "Expected " & Token_ID'Image(Match.ID) & " but found " &
            Token_ID'Image(Instance(Next_Token).ID));
      end if;

      Find_Next (Analyzer   => Analyzer,
                 Look_Ahead => not Actively
                 );
   exception
      when Constraint_Error =>
         Ada.Exceptions.Raise_Exception
           (Parse_Error'Identity,
            "Expected a token of type" & Ada.Tags.Expanded_Name(Instance'Tag) & " but found a " &
            Ada.Tags.Expanded_Name(Next_Token'Tag));
   end Parse;

   ----------------------------------------------------------------------------
   -- This routine should be a quick routine to verify that the given token
   -- can possibly succesfully parse. This routine is meant to be used for
   -- choosing between parsing options, so it should be a *very* quick check
   -- rather than a full parse.
   -- This version just checks against the currently loaded token on in the
   -- analyzer.
   ----------------------------------------------------------------------------
   function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in  Source_Class
     ) return Boolean is
   begin
      return Instance(Get (Analyzer)).ID = Match.ID;
   end Could_Parse_To;

end OpenToken.Token.Enumerated;
