-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- OpenToken Recursive Descent implementtion

with OpenToken.Token.List;
with OpenToken.Token.Sequence;
with OpenToken.Token.Selection;
with OpenToken.Recognizer.Keyword, OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;



pragma Elaborate_All (OpenToken.Token.Enumerated,
                      OpenToken.Token.Enumerated.Analyzer,
                      OpenToken.Recognizer.Keyword,
                      OpenToken.Recognizer.Separator,
                      OpenToken.Recognizer.Identifier,
                      OpenToken.Recognizer.Character_Set,
                      OpenToken.Recognizer.Line_Comment,
                      OpenToken.Recognizer.End_Of_File);

-- Package: Archicheck.Rules_Lexer specification

private package Archicheck.Rules_Lexer is

   pragma Elaborate_Body; --**

   -- 1 - Define an enumeration containing all your tokens.
   --     The complete list of tokens. No non-terminals in recursive descent.
   -- Rule -> Component contains Unit

   type Rules_Token is
      (-- Reserved words
--        A_Id,
--        Also_Id,
--        And_Id,
      Contains_Id,
--        Is_Id,
--        Layer_Id,
--        May_Id,
--        Over_Id,
--        Use_Id,
      -- Delimiters
--        Comma_Id, -- ,
--        Dot_Id, -- .
      EoL_Id,
--        Minus_Id,
--        Semicolon_Id, -- ;
--        -- Other tokens
--        Comment_Id,
      Identifier_Id,
--        Component_Name_Id,
      Whitespace_Id,
      -- Syntax error
      -- Bad_Token_T,
      --
      EoF_Id);

   -- 2 - Instantiate a token analyzer class for your tokens.
   package Master_Token is new OpenToken.Token.Enumerated
     (Token_IDs, Token_IDs'First, Token_IDs'Last, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;

   use OpenToken.Recognizer;

   -- 3 - Create a token recognizer for each token, and
   -- 4 - Map the recognizers to their tokens to create a syntax.
   Syntax : constant Tokenizer.Syntax :=
     (A_Id          => Tokenizer.Get (Keyword.Get ("a")),
      Also_Id       => Tokenizer.Get (Keyword.Get ("also")),
      And_Id        => Tokenizer.Get (Keyword.Get ("and")),
      Is_Id         => Tokenizer.Get (Keyword.Get ("is")),
      Contains_Id   => Tokenizer.Get (Keyword.Get ("contains")),
      Layer_Id      => Tokenizer.Get (Keyword.Get ("layer")),
      May_Id        => Tokenizer.Get (Keyword.Get ("may")),
      Over_Id       => Tokenizer.Get (Keyword.Get ("over")),
      Use_Id        => Tokenizer.Get (Keyword.Get ("use")),
      Comma_Id      => Tokenizer.Get (Separator.Get (",")),
      Dot_Id        => Tokenizer.Get (Separator.Get (".")),
      EoL_Id        => Tokenizer.Get (Separator.Get ((1 => OpenToken.EOL_Character))),
      Minus_Id      => Tokenizer.Get (Separator.Get ("-")),
      Semicolon_Id  => Tokenizer.Get (Separator.Get (";")),
      Identifier_Id => Tokenizer.Get (Identifier.Get),
      Component_Name_Id => Tokenizer.Get (Identifier.Get),
      Comment_Id        => Tokenizer.Get (Line_Comment.Get ("--")),
      Whitespace_Id => Tokenizer.Get (Character_Set.Get (Character_Set.Standard_Whitespace)),
      EoF_Id        => Tokenizer.Get (End_Of_File.Get));

      -- all terminal tokens
      A_T          : constant Master_Token.Handle := Syntax (A_Id).Token_Handle;
      Also_T       : constant Master_Token.Handle := Syntax (Also_Id).Token_Handle;
      And_T        : constant Master_Token.Handle := Syntax (And_Id).Token_Handle;
      Is_T         : constant Master_Token.Handle := Syntax (Is_Id).Token_Handle;
      Contains_T   : constant Master_Token.Handle := Syntax (Contains_Id).Token_Handle;
      Layer_T      : constant Master_Token.Handle := Syntax (Layer_Id).Token_Handle;
      May_T        : constant Master_Token.Handle := Syntax (May_Id).Token_Handle;
      Over_T       : constant Master_Token.Handle := Syntax (Over_Id).Token_Handle;
      Use_T        : constant Master_Token.Handle := Syntax (Use_Id).Token_Handle;
      Comma_T      : constant Master_Token.Handle := Syntax (Comma_Id).Token_Handle;
      Dot_T        : constant Master_Token.Handle := Syntax (Dot_Id).Token_Handle;
      Minus_T      : constant Master_Token.Handle := Syntax (Minus_Id).Token_Handle;
      Semicolon_T  : constant Master_Token.Handle := Syntax (Semicolon_Id).Token_Handle;
      Identifier_T : constant Master_Token.Handle := Syntax (Identifier_Id).Token_Handle;
      Component_Name_T : constant Master_Token.Handle := Syntax (Component_Name_Id).Token_Handle;
      Comment_T    : constant Master_Token.Handle := Syntax (Comment_Id).Token_Handle;
      EoF_T        : constant Master_Token.Handle := Syntax (EoF_Id).Token_Handle;
      EoL_T        : constant Master_Token.Handle := Syntax (EoL_Id).Token_Handle;

   --  Allow infix operators for building productions
   use type OpenToken.Token.Selection.Instance;
   use type OpenToken.Token.Sequence.Instance;
   use OpenToken.Token;


   R : constant Selection.Handle := new Selection.Instance;
   L : constant Selection.Handle := Selection.New_Instance (Sequence.New_Instance (Asterix & R) or ID);
   S : constant Selection.Handle := Selection.New_Instance (Sequence.New_Instance (L & Equals & R) or R);
   --  This is of type OpenToken.Token.Handle, so it can be passed to
   --  OpenToken.Token.Parse, rather than Sequence.Parse.
   S_Prime : constant OpenToken.Token.Handle := OpenToken.Token.Handle (Sequence.New_Instance (S & EOF));






   -- ...and nonterminals.
   Rule : constant OpenToken.Token.Handle := new OpenToken.Token.Selection.Instance;

   -- 5 - Create a token analyzer object initialized with your syntax
   Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);


   type Sentence_Token is abstract new OpenToken.Token.Instance with null record;
--     function Could_Parse_To (Match    : in Instance;
--                              Analyzer : in Source_Class) return Boolean;

   type Component_Definition_Token is abstract new OpenToken.Token.Instance with record
      Component_Name : Ada.Strings.Unbounded.Unbounded_String;
      Unit_List      : Unit_Lists.List;
      Source_List    : Archicheck.Source_Lists.List;
   end record;
   type Component_Definition_Token_Handle is access all Component_Definition_Token'Class;

   type Rule_Token is abstract new OpenToken.Token.Instance with record
      Server_Name : Ada.Strings.Unbounded.Unbounded_String;
      Client_Name : Ada.Strings.Unbounded.Unbounded_String;
      --** pour l'instant seule la regle layer est traitée
   end record;
   type Rule_Token_Handle is access all Rule_Token'Class;

   type File_List_Token is new OpenToken.Token.List.Instance with record
      Source_List : Archicheck.Source_Lists.List;
   end record; -- (File_List_Token, File_List_Token);
--     procedure Add_List_Element (Match   : in out File_List_Token;
--                                 Element : in out OpenToken.Token.Class);


end Archicheck.Rules_Lexer;
