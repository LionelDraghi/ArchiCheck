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

-------------------------------------------------------------------------------
--  This package defines a reusable list token. A list is a token that is made
--  up of any number of repetitions of other tokens, separated by a given
--  separator token.
-------------------------------------------------------------------------------
package body OpenToken.Token.List_Mixin is


   ----------------------------------------------------------------------------
   --  Retrieve a list token from the analyzer.
   --  The private routine Add_List_Element is called with every successive list
   --  element that is recognized. The private routine Build_List is called when
   --  the entire list has been recognized.
   --  An a non active parse does not comsume any input from the analyzer,
   --  and does not call any of the private routines.
   ----------------------------------------------------------------------------
   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     ) is

      Local_Match : Instance'Class := Match;
   begin
      Initialize (Local_Match);

      loop
         OpenToken.Token.Parse
           (Match    => OpenToken.Token.Class (Local_Match.Element.all),
            Analyzer => Analyzer,
            Actively => Actively
            );

         if Actively then
            Add_List_Element
              (Match   => Local_Match,
               Element => Local_Match.Element.all
               );
         end if;

         exit when not
           OpenToken.Token.Could_Parse_To
           (Match    => Local_Match.Separator.all,
            Analyzer => Analyzer
            );

         OpenToken.Token.Parse
           (Match    => Local_Match.Separator.all,
            Analyzer => Analyzer,
            Actively => Actively
            );

      end loop;

      if Actively then
         Build (Local_Match);
         Instance'Class (Match) := Local_Match;
      end if;
   end Parse;

   ----------------------------------------------------------------------------
   --  Construct a new list token, using the given Element and Separator tokens.
   ----------------------------------------------------------------------------
   function Get
     (Element   : access Component_Token'Class;
      Separator : access OpenToken.Token.Class
     ) return Instance is

      Result : Instance;
   begin
      Result.Element   := Component_Handle (Element);
      Result.Separator := OpenToken.Token.Handle (Separator);
      return Result;
   end Get;

   ----------------------------------------------------------------------------
   --  This routine should is a quick check to verify that the given list token
   --  can possibly succesfully parse from what's sitting in the analyzer.
   --  This routine is meant to be used for choosing between parsing options.
   --  It simply checks Could_Parse_To for this token's Left token.
   ----------------------------------------------------------------------------
   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class
     ) return Boolean is
   begin
      return Could_Parse_To (Match.Element.all, Analyzer);
   end Could_Parse_To;

end OpenToken.Token.List_Mixin;
