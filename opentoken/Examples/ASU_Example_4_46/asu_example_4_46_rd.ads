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
with Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;

with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Token.Selection;

-------------------------------------------------------------------------------
--  This package contains the library-level objects for the recursive-decent
--  version of Example 4.46 from the Dragon Book.
-------------------------------------------------------------------------------
package ASU_Example_4_46_RD is

   --  The complete list of tokens, with the terminals listed first.
   type Token_IDs is (Asterix_ID, ID_ID, Equals_ID, EOF_ID, Whitespace_ID);

   --  Instantiate all the nessecary packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);

   --  Define a lexer syntax for the terminals
   Syntax : constant Tokenizer.Syntax :=
     (Asterix_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("*")),
      ID_ID         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("id")),
      Equals_ID     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("=")),
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                      (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   --  Define all our terminal tokens
   Asterix : constant Master_Token.Handle := Syntax (Asterix_ID).Token_Handle;
   ID      : constant Master_Token.Handle := Syntax (ID_ID).Token_Handle;
   Equals  : constant Master_Token.Handle := Syntax (Equals_ID).Token_Handle;
   EOF     : constant Master_Token.Handle := Syntax (EOF_ID).Token_Handle;

   --  The tokens. Since the defintion of tokens L and R are mutually-recursive, we'll have to
   --  delay their initializations (grammar specification) until after they are both declared.
   S_Prime :          OpenToken.Token.Handle;
   S       : constant OpenToken.Token.Handle := new OpenToken.Token.Selection.Instance;
   L       : constant OpenToken.Token.Handle := new OpenToken.Token.Selection.Instance;
   R       : constant OpenToken.Token.Handle := new OpenToken.Token.Selection.Instance;

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Input_File'Unchecked_Access);

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

end ASU_Example_4_46_RD;
