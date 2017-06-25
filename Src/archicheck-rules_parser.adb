-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with OpenToken.Token; use OpenToken.Token;
with Ada.Exceptions;
with OpenToken.Text_Feeder.Text_IO;
with Ada.Text_IO;
with OpenToken.Text_Feeder;
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Character_Set;
with Ada.Strings.Maps.Constants;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Recognizer.Keyword;
with Archicheck.IO;

package body Archicheck.Rules_Parser is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False;
                             Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug (Msg    : in String  := "";
   --                          Debug  : in Boolean := True;
   --                          Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug;


   -- The list of tokens, without non-terminals in recursive descent.
   type Token_Ids is
     (Whitespace_Id,

      -- Reserved words
      A_Id, -- first terminal
      --        Also_Id,
      --        And_Id,
      Contains_Id,
      Is_Id,
      Layer_Id,
      --        May_Id,
      Over_Id,
      --        Use_Id,

      -- Delimiters
      --        Comma_Id, -- ,
      --        Dot_Id, -- .
      EoF_Id,
      --        EoL_Id,
      --        Minus_Id,
      --        Semicolon_Id, -- ;
      --  Identifier must be after keywords, so they are recognized instead
      Identifier_Id, -- last terminal

      --  non-terminals---------------
      -- Comment_Id,
      --Bad_Token_Id
      Rules_File_Id,
      Rule_List_Id,
      Rule_Id,
      Component_Declaration_Id,
      Layer_Declaration_Id
     );

   -- private

   package Master_Token is new OpenToken.Token.Enumerated
     (Token_Ids, A_Id, Identifier_Id, Token_Ids'Image);
   package Tokenizer is new Master_Token.Analyzer;

   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   package Parser is new Production.Parser (Tokenizer);

   package LALRs is new Parser.LALR (First_State_Index => 1);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label);
   package LALR_Parser is new LALRs.Parser (First_Parser_Label, Parser_Lists);
   Token_Image_Width : constant Integer := Token_Ids'Width;
   package LALR_Generator is new LALRs.Generator (Token_Image_Width, Production_List);


   -- Step 3: Map the Terminal Token ID's to their recognizers and tokens
   --use OpenToken.Recognizer;
   Syntax : constant Tokenizer.Syntax :=
              (Whitespace_Id => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
               A_Id          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a")),
               -- Also_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("also")),
               -- And_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
               Contains_Id   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("contains")),
               Is_Id         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is")),
               Layer_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("layer")),
               -- May_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("may")),
               Over_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("over")),
               -- Use_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use")),
               -- Delimiters
               -- Comma_Id      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
               -- Dot_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
               -- Minus_Id      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-")),
               -- Semicolon_Id  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

               --  Identifier must be after keywords, so they are recognized instead
               Identifier_Id => Tokenizer.Get (OpenToken.Recognizer.Identifier.Get (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
                                                                                    Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
               -- EoL_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ((1 => OpenToken.EOL_Character))),
               -- Comment_Id    => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--")),
               -- Bad_Token_Id  => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
               EoF_Id        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   --  The non-terminal tokens are pointers (type Handle) to allow for
   --  mutual recursion. So the terminal ones are too, for
   --  consistency, and to reduce the number of '&' and 'or' operators
   --  we need.


   --  Terminal tokens
   A_T        : constant Master_Token.Class := Master_Token.Get (A_Id);
   Contains   : constant Master_Token.Class := Master_Token.Get (Contains_Id);
   Identifier : constant Master_Token.Class := Master_Token.Get (Identifier_Id);
   Is_T       : constant Master_Token.Class := Master_Token.Get (Is_Id);
   EoF        : constant Master_Token.Class := Master_Token.Get (EoF_Id);
   -- EoL        : constant Master_Token.Class := Master_Token.Get (EoL_Id);
   Layer      : constant Master_Token.Class := Master_Token.Get (Layer_Id);
   Over       : constant Master_Token.Class := Master_Token.Get (Over_Id);

   -- Non-terminal tokens, which define the grammar.
   Layer_Declaration     : constant Nonterminal.Class := Nonterminal.Get (Layer_Declaration_Id);
   Component_Declaration : constant Nonterminal.Class := Nonterminal.Get (Component_Declaration_Id);
   Rule                  : constant Nonterminal.Class := Nonterminal.Get (Rule_Id);
   Rule_List             : constant Nonterminal.Class := Nonterminal.Get (Rule_List_Id);
   Rules_File            : constant Nonterminal.Class := Nonterminal.Get (Rules_File_Id);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   -- Grammar:
   --
   -- Rules_File            -> Rule_List & EoF
   -- Rule_List             -> Rule [Rule]*
   -- Rule                  -> Component_Declaration | Layer_Declaration
   -- Component_Declaration -> Component contains Unit                      Insert [Component, Unit] in Component_List
   -- Layer_Declaration     -> Layer is a layer over Layer                  Insert [Layer, Layer]    in Layer_List

   -- -------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID);

   Grammar : constant Production_List.Instance :=
               Rules_File             <= Rule_List & EoF                    and
                   Rule_List              <= Rule_List & Rule                   and
                       Rule_List              <= Rule                               and
                           Rule                   <= Layer_Declaration                  and
                               Rule                   <= Component_Declaration              and
                                   Component_Declaration  <= Identifier & Contains & Identifier                  + Store_Component_Declaration'Access and -- Nonterminal.Synthesize_Self and -- Insert [Component, Unit] in Component_List
                                       Layer_Declaration      <= Identifier & Is_T & A_T & Layer & Over & Identifier + Store_Layer_Declaration'Access;    -- Insert [Layer, Layer]    in Layer_List an

   --  This is of type OpenToken.Token.Handle, so it can be passed to
   --  OpenToken.Token.Parse, rather than Sequence.Parse.

   -- Units_In_File definition : "units in Dir1/*.ads"
   --     Units_In_File.all := Selection.Class
   --       (File or
   --        Sequence.New_Instance (File & Selection.Class (And_T or Comma_T)));
   -- File_List : File [(and|,) File]
   -- example : units in Dir1/*.ads, units in Dir2 and units in Dir3"



   -- Step 5: Define the Lexical Analyser



   --     type Sentence_Token is abstract new OpenToken.Token.Instance with null record;
   --     --     function Could_Parse_To (Match    : in Instance;
   --     --                              Analyzer : in Source_Class) return Boolean;
   --
   --     type Component_Definition_Token is abstract new OpenToken.Token.Instance with record
   --        Component_Name : Ada.Strings.Unbounded.Unbounded_String;
   --        Unit_List      : Unit_Lists.List;
   --        Source_List    : Archicheck.Source_Lists.List;
   --     end record;
   --     type Component_Definition_Token_Handle is access all Component_Definition_Token'Class;
   --
   --     type Rule_Token is abstract new OpenToken.Token.Instance with record
   --        Server_Name : Ada.Strings.Unbounded.Unbounded_String;
   --        Client_Name : Ada.Strings.Unbounded.Unbounded_String;
   --        --** pour l'instant seule la regle layer est traitée
   --     end record;
   --     type Rule_Token_Handle is access all Rule_Token'Class;
   --
   --     --     type File_List_Token is new OpenToken.Token.List.Instance with record
   --     --        Source_List : Archicheck.Source_Lists.List;
   --     --     end record; -- (File_List_Token, File_List_Token);
   --     --     procedure Add_List_Element (Match   : in out File_List_Token;
   --     --                                 Element : in out OpenToken.Token.Class);




   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
                  OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer   : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  The lalr parser instance.
   Rules_File_Parser : LALR_Parser.Instance := LALR_Parser.Initialize (Analyzer,
                                                                       LALR_Generator.Generate (Grammar));


   -- -------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      -- use Ada.Text_IO;
   begin
      Token_List.Next_Token (Right); -- move "Right" over "contains"
      Token_List.Next_Token (Right); -- move "Right" over the Unit

      Put_Debug_Line;
      --        Put_Debug_Line ("Store_Component_Declaration : "
      --                        & Value (Token_List.Token_Handle (Right).all));
      Put_Debug_Line ("Store_Component_Declaration : "
                      & Token_Ids'Image (Master_Token.ID (Token_List.Token_Handle (Left).all))
                      & " contains "
                      & Token_Ids'Image (Master_Token.ID (Token_List.Token_Handle (Right).all)));
      Put_Debug_Line;

   end Store_Component_Declaration;

   -- -------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      -- use Ada.Text_IO;
   begin
      Token_List.Next_Token (Right); -- move "Right" over "is"
      Token_List.Next_Token (Right); -- move "Right" over "a"
      Token_List.Next_Token (Right); -- move "Right" over "layer"
      Token_List.Next_Token (Right); -- move "Right" over "over"
      Token_List.Next_Token (Right); -- move "Right" over the layer

      Put_Debug_Line;
      --        Put_Debug_Line ("Store_Component_Declaration : "
      --                        & Value (Token_List.Token_Handle (Right).all));
      Put_Debug_Line ("Store_Layer_Declaration : "
                      & Token_Ids'Image (Master_Token.ID (Token_List.Token_Handle (Left).all))
                      & " is a layer over "
                      & Token_Ids'Image (Master_Token.ID (Token_List.Token_Handle (Right).all)));
      Put_Debug_Line;

   end Store_Layer_Declaration;

   -- -------------------------------------------------------------------------
   procedure Parse (File_Name : in String) is
      use Ada.Text_IO;

   begin
      Put_Debug_Line;
      Put_Debug_Line ("Opening " & File_Name);

      Open (Input_File,
            Mode => In_File,
            Name => File_Name);
      Set_Input (Input_File);

      Put_Debug_Line;
      Put_Debug_Line ("Parsing");

      -- LALR_Parser.Print_Table (Test_Parser);
      Parsing : begin
         LALR_Parser.Parse (Rules_File_Parser);

      exception
         when Error : others =>
            IO.Put_Error (File_Name & ":" & Integer'Image (Analyzer.Line) &
                            ":" & Integer'Image (Analyzer.Column) &
                            ": parse exception");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));

      end Parsing;

      Close (File => Input_File);

   end Parse;

end Archicheck.Rules_Parser;
