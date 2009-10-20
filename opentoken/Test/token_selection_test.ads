--  Abstract :
--
--  Test OpenToken.Token.Selection
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;

package Token_Selection_Test is

   type Token_IDs is (Do_ID, Several, Things, Int, Times, In_ID, A_ID, Row, EOF, Whitespace);

   package Terminal_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Terminal_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (Do_ID      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("do")),
      Several    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("several")),
      Things     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("things")),
      Int        => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
      Times      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("times")),
      In_ID      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("in")),
      A_ID       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a")),
      Row        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("row")),
      EOF        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                   (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Do_Keyword      : Terminal_Token.Handle := Syntax (Do_ID).Token_Handle;
   Several_Keyword : Terminal_Token.Handle := Syntax (Several).Token_Handle;
   Things_Keyword  : Terminal_Token.Handle := Syntax (Things).Token_Handle;
   Int_Literal     : Terminal_Token.Handle := Syntax (Int).Token_Handle;
   Times_Keyword   : Terminal_Token.Handle := Syntax (Times).Token_Handle;
   In_Keyword      : Terminal_Token.Handle := Syntax (In_ID).Token_Handle;
   A_Keyword       : Terminal_Token.Handle := Syntax (A_ID).Token_Handle;
   Row_Keyword     : Terminal_Token.Handle := Syntax (Row).Token_Handle;

   Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

end Token_Selection_Test;
