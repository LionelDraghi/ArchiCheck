-------------------------------------------------------------------------------
--
-- Copyright (C) 1999, 2008 Christoph Karl Walter Grein
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

with Ada.Strings.Maps.Constants;

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Keyword, OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Graphic_Character, OpenToken.Recognizer.String;
with OpenToken.Recognizer.Integer, OpenToken.Recognizer.Based_Integer_Ada_Style,
  OpenToken.Recognizer.Real, OpenToken.Recognizer.Based_Real_Ada_Style;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.Nothing;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.Text_IO;

pragma Elaborate_All (OpenToken.Token.Enumerated, OpenToken.Token.Enumerated.Analyzer,
                      OpenToken.Recognizer.Keyword, OpenToken.Recognizer.Separator,
                      OpenToken.Recognizer.Identifier,
                      OpenToken.Recognizer.Graphic_Character, OpenToken.Recognizer.String,
                      OpenToken.Recognizer.Integer, OpenToken.Recognizer.Based_Integer_Ada_Style,
                      OpenToken.Recognizer.Real, OpenToken.Recognizer.Based_Real_Ada_Style,
                      OpenToken.Recognizer.Character_Set,
                      OpenToken.Recognizer.Line_Comment,
                      OpenToken.Recognizer.Nothing,
                      OpenToken.Recognizer.End_Of_File,
                      OpenToken.Text_Feeder.Text_IO);

package body Ada_Lexer is

   package Master_Ada_Token is new OpenToken.Token.Enumerated (Ada_Token);
   package Tokenizer        is new Master_Ada_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (Abort_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abort")),
      Abs_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abs")),
      Abstract_T     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abstract")),
      Accept_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("accept")),
      Access_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("access")),
      Aliased_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("aliased")),
      All_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("all")),
      And_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
      Array_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("array")),
      At_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("at")),
      Begin_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("begin")),
      Body_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("body")),
      Case_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("case")),
      Constant_T     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("constant")),
      Declare_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("declare")),
      Delay_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("delay")),
      Delta_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("delta")),
      Digits_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("digits")),
      Do_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("do")),
      Else_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("else")),
      Elsif_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("elsif")),
      End_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("end")),
      Entry_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("entry")),
      Exception_T    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("exception")),
      Exit_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("exit")),
      For_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("for")),
      Function_T     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("function")),
      Generic_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("generic")),
      Goto_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("goto")),
      If_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("if")),
      In_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("in")),
      Interface_T    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("interface")),
      Is_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is")),
      Limited_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("limited")),
      Loop_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("loop")),
      Mod_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("mod")),
      New_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("new")),
      Not_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("not")),
      Null_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("null")),
      Of_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("of")),
      Or_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("or")),
      Others_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("others")),
      Out_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("out")),
      Overriding_T   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("overriding")),
      Package_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("package")),
      Pragma_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("pragma")),
      Private_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("private")),
      Procedure_T    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("procedure")),
      Protected_T    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("protected")),
      Raise_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("raise")),
      Range_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("range")),
      Record_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("record")),
      Rem_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("rem")),
      Renames_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("renames")),
      Requeue_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("requeue")),
      Return_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("return")),
      Reverse_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("reverse")),
      Select_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("select")),
      Separate_T     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("separate")),
      Subtype_T      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("subtype")),
      Synchronized_T => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("synchronized")),
      Tagged_T       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("tagged")),
      Task_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("task")),
      Terminate_T    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("terminate")),
      Then_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("then")),
      Type_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("type")),
      Until_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("until")),
      Use_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use")),
      When_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("when")),
      While_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("while")),
      With_T         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("with")),
      Xor_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("xor")),
      Colon_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":")),
      Comma_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      Dot_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
      Semicolon_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),
      Tick_T                => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("'")),
      Left_Parenthesis_T    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Right_Parenthesis_T   => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      Concatenate_T         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("&")),
      Alternative_T         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("|")),
      Equal_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=")),
      Not_Equal_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("/=")),
      Greater_Than_T        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">")),
      Less_Than_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<")),
      Greater_Equal_T       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">=")),
      Less_Equal_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<=")),
      Plus_T                => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+")),
      Minus_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-")),
      Times_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("*")),
      Divide_T              => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("/")),
      Arrow_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=>")),
      Assignment_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":=")),
      Double_Dot_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("..")),
      Exponentiate_T        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("**")),
      Left_Label_Bracket_T  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<<")),
      Right_Label_Bracket_T => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">>")),
      Box_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<>")),
      Integer_T       => Tokenizer.Get (OpenToken.Recognizer.Integer.Get (Allow_Signs => False)),
      Based_Integer_T => Tokenizer.Get (OpenToken.Recognizer.Based_Integer_Ada_Style.Get),
      Real_T          => Tokenizer.Get (OpenToken.Recognizer.Real.Get (Allow_Signs => False)),
      Based_Real_T    => Tokenizer.Get (OpenToken.Recognizer.Based_Real_Ada_Style.Get),
      Character_T     => Tokenizer.Get (OpenToken.Recognizer.Graphic_Character.Get),
      String_T        => Tokenizer.Get (OpenToken.Recognizer.String.Get),
      Identifier_T    => Tokenizer.Get (OpenToken.Recognizer.Identifier.Get),
      Comment_T       => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--")),
      Whitespace_T    => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Bad_Token_T     => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
      End_of_File_T   => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax);

   procedure Set_Input_Feeder (File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;
   end Set_Input_Feeder;

   procedure Exception_on_Syntax_Error is
   begin
      Tokenizer.Unset_Default (Analyzer);
   end Exception_on_Syntax_Error;

   procedure Bad_Token_on_Syntax_Error is
   begin
      Tokenizer.Set_Default (Analyzer, Bad_Token_T);
   end Bad_Token_on_Syntax_Error;

   procedure Set_Comments_Reportable (To : in Boolean) is
   begin
      Syntax (Comment_T).Recognizer.Report := To;
   end Set_Comments_Reportable;

   Exclusion : constant array (Boolean) of Ada.Strings.Maps.Character_Set :=  -- see Find_Next
     (False => Ada.Strings.Maps.Null_Set,                -- character literal enabled
      True  => Ada.Strings.Maps.Constants.Graphic_Set);  --                   disabled

   procedure Find_Next is
      --  Take care that the expression Character'('x') is correctly processed:
      --  A character literal cannot follow an identifier.
   begin
      Tokenizer.Find_Next (Analyzer, Look_Ahead => False);
      OpenToken.Recognizer.Graphic_Character.Redefine
        (OpenToken.Recognizer.Graphic_Character.Instance (Syntax (Character_T).Recognizer.all),
         Exclusion (Token_ID = Identifier_T));
   end Find_Next;

   function Line return Natural is
   begin
      return Tokenizer.Line (Analyzer);
   end Line;

   function Column return Natural is
   begin
      return Tokenizer.Column (Analyzer);
   end Column;

   function Token_ID return Ada_Token is
   begin
      return Tokenizer.ID (Analyzer);
   end Token_ID;

   function Lexeme return String is
   begin
      return Tokenizer.Lexeme (Analyzer);
   end Lexeme;

end Ada_Lexer;
