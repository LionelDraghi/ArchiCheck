-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


--with OpenToken.Token.List;
--with OpenToken.Token.Sequence;
--with OpenToken.Token.Selection;
with OpenToken.Recognizer.Keyword, OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Character_Set;
--with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with Ada.Strings.Maps.Constants;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated.Identifier;

pragma Elaborate_All (OpenToken.Token.Enumerated,
                      OpenToken.Token.Enumerated.Analyzer,
                      OpenToken.Recognizer.Keyword,
                      OpenToken.Recognizer.Separator,
                      OpenToken.Recognizer.Identifier,
                      OpenToken.Recognizer.Character_Set,
                      --OpenToken.Recognizer.Line_Comment,
                      OpenToken.Recognizer.End_Of_File);

-- Package: Archicheck.Rules_Parser specification

private package Archicheck.Rules_Parser is

   pragma Elaborate_Body; --**

   -- 1 - Define an enumeration containing all your tokens.
   type Token_Ids is
     (--Terminals------------------

      -- Reserved words
      A_Id,
      Also_Id,
      And_Id,
      Contains_Id,
      Is_Id,
      Layer_Id,
      May_Id,
      Over_Id,
      Use_Id,
      -- Delimiters
      Comma_Id, -- ,
      Dot_Id, -- .
      EoL_Id,
      Minus_Id,
      Semicolon_Id, -- ;

      --  Identifier must be after keywords, so they are recognized instead
      Identifier_Id,

      Whitespace_Id,
      EoF_Id,
      -- ACHTUNG : last terminal must be the formal parameter of
      --           Tokenizer package instanciation here-after

      --  non-terminals---------------
      -- Other tokens
      Comment_Id,
      Rules_Id

      );

   -- 2 - Step 2: Instantiate Your Token Packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_Ids);
   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => EoF_Id);

   --  Our terminal token types.
   package Identifier_Token is new Master_Token.Identifier;


   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

-- Allow infix operators for building productions
   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   --Step 3: Create the Tokens
      A : aliased Master_Token.Class := Master_Token.Get (A_Id);
      Aussi : aliased Master_Token.Class := Master_Token.Get (Also_Id);
      Et : aliased Master_Token.Class := Master_Token.Get (And_Id);
      Contains : aliased Master_Token.Class := Master_Token.Get (Contains_Id);
      Est : aliased Master_Token.Class := Master_Token.Get (Is_Id);
      Layer : aliased Master_Token.Class := Master_Token.Get (Layer_Id);
      may : aliased Master_Token.Class := Master_Token.Get (May_Id);
      over : aliased Master_Token.Class := Master_Token.Get (Over_Id);
      utilise : aliased Master_Token.Class := Master_Token.Get (Use_Id);
      comma : aliased Master_Token.Class := Master_Token.Get (Comma_Id);
      dot : aliased Master_Token.Class := Master_Token.Get (Dot_Id);
      EoL : aliased Master_Token.Class := Master_Token.Get (EoL_Id);
      Minus : aliased Master_Token.Class := Master_Token.Get (Minus_Id);
      Semicolon : aliased Master_Token.Class := Master_Token.Get (Semicolon_Id);
      Identifier : aliased Master_Token.Class := Master_Token.Get (Identifier_Id);
      Whitespace : aliased Master_Token.Class := Master_Token.Get (Whitespace_Id);
      EoF : aliased Master_Token.Class := Master_Token.Get (EoF_Id);
      Comment : aliased Nonterminal.Class  := Nonterminal.Get (Comment_Id);
      Rules : aliased Nonterminal.Class  := Nonterminal.Get (Rules_Id);

   --Step 4: Map the Terminal Token ID's to their recognizers and tokens
   Syntax : constant Tokenizer.Syntax :=
     (A_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a"),
                                Master_Token.Get (Name => "a")),
      Also_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("also"),
                                   Master_Token.Get (Name => "aussi")),
      And_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and"),
                                   Master_Token.Get (Name => "et")),
      Contains_Id => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("contains"),
                                   Master_Token.Get (Name => "contains")),
      Is_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is"),
                                   Master_Token.Get (Name => "est")),
      Layer_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("layer"),
                                   Master_Token.Get (Name => "layer")),
      May_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("may"),
                                   Master_Token.Get (Name => "may")),
      Over_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("over"),
                                   Master_Token.Get (Name => "over")),
      Use_Id    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use"),
                                   Master_Token.Get (Name => "utilise")),
      -- Delimiters
      Comma_Id    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (","),
                                   Master_Token.Get (Name => ",")),
      Dot_Id    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("."),
                                   Master_Token.Get (Name =>  ".")),
      Minus_Id    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-"),
                                   Master_Token.Get (Name => "-")),
      Semicolon_Id    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";"),
                                      Master_Token.Get (Name => ";")),

      --  Identifier must be after keywords, so they are recognized instead
      Identifier_Id => Tokenizer.Get
        (Recognizer => OpenToken.Recognizer.Identifier.Get
           (Body_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set),
         New_Token  => Identifier_Token.Get (Identifier_Id)),

   EoF_Id        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, EoF),
      EoL_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get
        ((1 => OpenToken.EOL_Character))),
   Whitespace_Id => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
        (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
   );

  -- Step 5: Define a Lexical Analyzer
   Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   -- Step 6: Creating a Grammar












--        EoL_Id        => Tokenizer.Get (Separator.Get ((1 => OpenToken.EOL_Character))),
--        Comment_Id        => Tokenizer.Get (Line_Comment.Get ("--")),
--        Whitespace_Id => Tokenizer.Get (Character_Set.Get (Character_Set.Standard_Whitespace)),


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

--     type File_List_Token is new OpenToken.Token.List.Instance with record
--        Source_List : Archicheck.Source_Lists.List;
--     end record; -- (File_List_Token, File_List_Token);
--     procedure Add_List_Element (Match   : in out File_List_Token;
--                                 Element : in out OpenToken.Token.Class);


end Archicheck.Rules_Parser;
