-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Rules.Parser body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with OpenToken.Token; use OpenToken.Token;
with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Text_Feeder;
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Identifier;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.Keyword;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;

with Archicheck.IO;
with Archicheck.Units;
with Archicheck.Settings;
with Archicheck.Sources; use Archicheck.Sources;

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Maps.Constants;

package body Archicheck.Rules.Parser is

   -- Change default Debug parameter value to enable/disable
   -- Debug messages in this package
   -- --------------------------------------------------------------------------
   --  procedure Put_Debug_Line (Msg    : in String  := "";
   --                            Debug  : in Boolean := Settings.Debug_Mode; -- change to True to debug this package
   --                            Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug (Msg    : in String  := "";
   --                          Debug  : in Boolean := True;
   --                          Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug;

   -- NB : Steps here after are references to the OpenToken documentation,
   --      chapter Table-Driven Parsing.

   -- Step 1: Creating an Enumeration of Token IDs
   -- --------------------------------------------------------------------------

   Prefix : constant String := "Rules.Parser === ";
   
   -- The list of tokens, without non-terminals in recursive descent.
   type Token_Ids is
     (-- Non reporting tokens (not used in generating an LALR grammar) ---------
      Whitespace_Id,
      Ada_Comment_Id,
      Java_Comment_Id,
      Shell_Comment_Id,

      -- Terminals tokens ------------------------------------------------------
      -- Keywords
      A_Id, -- first terminal
      And_Id,
      Are_Id,
      Contains_Id,
      Independent_Id,
      Is_Id,
      Layer_Id,
      Only_Id,
      May_Id,
      Over_Id,
      Use_Id,
      Forbidden_Id,
      Allowed_Id,

      -- Misc
      Comma_Id,     -- ','
      Dot_Id,       -- '.'
      Star_Id,      -- '*'
      Semicolon_Id, -- ';'
      EoF_Id,
      --  Identifier must be after keywords, so they are recognized instead
      Identifier_Id, -- last terminal

      -- Non-terminals tokens --------------------------------------------------
      Rules_File_Id,
      Rule_List_Id,
      Rule_Id,
      Unit_Id,
      Unit_List_Id,
      Component_Decla_Id,
      Independent_Components_Decla_Id,
      Layer_Decla_Id,
      Use_Decla_Id,
      Restricted_Use_Decla_Id,
      Forbidden_Use_Decla_Id,
      Allowed_Used_Decla_Id
      Component_Decla_Id,
      Independent_Components_Decla_Id,
      Layer_Decla_Id,
      Use_Decla_Id,
      Restricted_Use_Decla_Id,
      Forbidden_Use_Decla_Id,
      Allowed_Used_Decla_Id
     );
   First_Terminal : constant Token_Ids := A_Id;
   Last_Terminal  : constant Token_Ids := Identifier_Id;

   -- Step 2: Instantiate Your Token Packages
   -- --------------------------------------------------------------------------

   package Master_Token is new OpenToken.Token.Enumerated
     (Token_Ids, First_Terminal, Last_Terminal, Token_Ids'Image);
   package Tokenizer is new Master_Token.Analyzer;

   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Identifiers is new Master_Token.Identifier;

   package Production is new OpenToken.Production (Master_Token,
                                                   Token_List,
                                                   Nonterminal);
   package Production_List is new Production.List;

   package Parser is new Production.Parser (Tokenizer);

   package LALRs is new Parser.LALR (First_State_Index => 1);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   First_Parser_Label : constant := 1;
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label);
   package LALR_Parser is new LALRs.Parser (First_Parser_Label,
                                            Parser_Lists);
   Token_Image_Width : constant Integer := Token_Ids'Width;
   package LALR_Generator is new LALRs.Generator (Token_Image_Width,
                                                  Production_List);

   -- Step 3: create the Token
   -- --------------------------------------------------------------------------

   -- Terminal tokens
   A_T           : constant Master_Token.Class := Master_Token.Get (A_Id);
   And_T         : constant Master_Token.Class := Master_Token.Get (And_Id);
   Are_T         : constant Master_Token.Class := Master_Token.Get (Are_Id);
   Contains_T    : constant Master_Token.Class := Master_Token.Get (Contains_Id);
   Comma_T       : constant Master_Token.Class := Master_Token.Get (Comma_Id);
   Dot_T         : constant Master_Token.Class := Master_Token.Get (Dot_Id);
   Star_T        : constant Master_Token.Class := Master_Token.Get (Star_Id);
   Identifier_T  : constant Master_Token.Class := Master_Token.Get (Identifier_Id);
   Independent_T : constant Master_Token.Class := Master_Token.Get (Independent_Id);
   Is_T          : constant Master_Token.Class := Master_Token.Get (Is_Id);
   EoF_T         : constant Master_Token.Class := Master_Token.Get (EoF_Id);
   Layer_T       : constant Master_Token.Class := Master_Token.Get (Layer_Id);
   Only_T        : constant Master_Token.Class := Master_Token.Get (Only_Id);
   May_T         : constant Master_Token.Class := Master_Token.Get (May_Id);
   Use_T         : constant Master_Token.Class := Master_Token.Get (Use_Id);
   Forbidden_T   : constant Master_Token.Class := Master_Token.Get (Forbidden_Id);
   Allowed_T     : constant Master_Token.Class := Master_Token.Get (Allowed_Id);
   Over_T        : constant Master_Token.Class := Master_Token.Get (Over_Id);
   Semicolon_T   : constant Master_Token.Class := Master_Token.Get (Semicolon_Id);

   -- Non-terminal tokens, which define the grammar.
   Use_Decla                    : constant Nonterminal.Class := Nonterminal.Get (Use_Decla_Id);
   Restricted_Use_Decla         : constant Nonterminal.Class := Nonterminal.Get (Restricted_Use_Decla_Id);
   Forbidden_Use_Decla          : constant Nonterminal.Class := Nonterminal.Get (Forbidden_Use_Decla_Id);
   Allowed_Use_Decla            : constant Nonterminal.Class := Nonterminal.Get (Allowed_Used_Decla_Id);
   Layer_Decla                  : constant Nonterminal.Class := Nonterminal.Get (Layer_Decla_Id);
   Component_Decla              : constant Nonterminal.Class := Nonterminal.Get (Component_Decla_Id);
   Independent_Components_Decla : constant Nonterminal.Class := Nonterminal.Get (Independent_Components_Decla_Id);
   Rule                         : constant Nonterminal.Class := Nonterminal.Get (Rule_Id);
   Rule_List                    : constant Nonterminal.Class := Nonterminal.Get (Rule_List_Id);
   Rules_File                   : constant Nonterminal.Class := Nonterminal.Get (Rules_File_Id);
   Unit                         : constant Nonterminal.Class := Nonterminal.Get (Unit_Id);
   Unit_List                    : constant Nonterminal.Class := Nonterminal.Get (Unit_List_Id);
   Use_Decla                    : constant Nonterminal.Class := Nonterminal.Get (Use_Decla_Id);
   Restricted_Use_Decla         : constant Nonterminal.Class := Nonterminal.Get (Restricted_Use_Decla_Id);
   Forbidden_Use_Decla          : constant Nonterminal.Class := Nonterminal.Get (Forbidden_Use_Decla_Id);
   Allowed_Use_Decla            : constant Nonterminal.Class := Nonterminal.Get (Allowed_Used_Decla_Id);
   Layer_Decla                  : constant Nonterminal.Class := Nonterminal.Get (Layer_Decla_Id);
   Component_Decla              : constant Nonterminal.Class := Nonterminal.Get (Component_Decla_Id);
   Independent_Components_Decla : constant Nonterminal.Class := Nonterminal.Get (Independent_Components_Decla_Id);
   Rule                         : constant Nonterminal.Class := Nonterminal.Get (Rule_Id);
   Rule_List                    : constant Nonterminal.Class := Nonterminal.Get (Rule_List_Id);
   Rules_File                   : constant Nonterminal.Class := Nonterminal.Get (Rules_File_Id);
   Unit                         : constant Nonterminal.Class := Nonterminal.Get (Unit_Id);
   Unit_List                    : constant Nonterminal.Class := Nonterminal.Get (Unit_List_Id);


   -- Step 4: Map the Terminal Token ID's to their recognizers and tokens
   -- --------------------------------------------------------------------------

   Syntax : constant Tokenizer.Syntax :=
              [Whitespace_Id    => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
              [Whitespace_Id    => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
               --
               A_Id             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a")),
               And_Id           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
               Are_Id           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("are")),
               Contains_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("contains")),
               Independent_Id   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("independent")),
               Is_Id            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is")),
               Layer_Id         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("layer")),
               Only_Id          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("only")),
               May_Id           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("may")),
               Over_Id          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("over")),
               Use_Id           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use")),
               Forbidden_Id     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("forbidden")),
               Allowed_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("allowed")),

               -- Delimiters
               Ada_Comment_Id   => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--", Reportable => False)),
               Java_Comment_Id  => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("//", Reportable => False)),
               Shell_Comment_Id => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("#",  Reportable => False)),
               Comma_Id         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
               Dot_Id           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
               Star_Id          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("*")),
               Semicolon_Id     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

               EoF_Id           => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),

               --  Identifier must be after keywords, so they are recognized instead
               Identifier_Id    => Tokenizer.Get
                 (Recognizer => OpenToken.Recognizer.Identifier.Get
                    (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
                     Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
                  New_Token  => Identifiers.Get (Identifier_Id))

               -- EoL_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ((1 => OpenToken.EOL_Character))),
              ];
              ];

   -- Step 5: Define a Lexical Analyzer
   -- --------------------------------------------------------------------------

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
                  OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer   : constant Tokenizer.Handle :=
                  Tokenizer.Initialize (Syntax, Feeder'Access);

   -- Step 6: Creating a Grammar
   -- --------------------------------------------------------------------------

   -- --------------------------------------------------------------------------
   procedure Initialize_Unit_Name
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Dot_Identifier
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Final_Star
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Component_Decla
   procedure Store_Component_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Layer_Decla
   procedure Store_Layer_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Use_Decla
   procedure Store_Use_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Restricted_Use_Decla
   procedure Store_Restricted_Use_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Forbidden_Unit
   procedure Add_Forbidden_Unit
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Independent_Components
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID);


   -- Grammar:
   -- --------------------------------------------------------------------------
   -- Rules_File                  -> Rule_List & EoF
   -- Rule_List                   -> Rule [Rule]*
   -- Separator                   -> , | and | blank
   -- Unit                        -> Identifier[.Identifier]*
   -- Unit_List                   -> Unit [[Separator]* Unit]*                    Unit_List = Unit & Unit & Unit etc.
   -- Component_Decla       -> Component contains Unit_List                 Insert [Component, Unit_List] in Component_List
   -- Layer_Decla           -> Layer is a layer over Layer                  Insert [Layer, Layer]         in Layer_List
   -- Restricted_Use_Decla  -> only Component1 may use component2
   -- Use_Decla             -> Component1 uses Component2
   -- Rule                        -> Independent_Components_Decla | Component_Decla | Layer_Decla | Use_Decla | Restricted_Use_Decla
   -- Component_Decla       -> Component contains Unit_List                 Insert [Component, Unit_List] in Component_List
   -- Layer_Decla           -> Layer is a layer over Layer                  Insert [Layer, Layer]         in Layer_List
   -- Restricted_Use_Decla  -> only Component1 may use component2
   -- Use_Decla             -> Component1 uses Component2
   -- Rule                        -> Independent_Components_Decla | Component_Decla | Layer_Decla | Use_Decla | Restricted_Use_Decla

   Grammar : constant Production_List.Instance :=
               Rules_File                  <= Rule_List & EoF_T              and

               Rule_List                   <= Rule_List & Semicolon_T        and
               Rule_List                   <= Rule_List & Rule               and
               Rule_List                   <= Rule                           and

               Rule                        <= Use_Decla                      and
               Rule                        <= Restricted_Use_Decla           and
               Rule                        <= Layer_Decla                    and
               Rule                        <= Forbidden_Use_Decla            and
               Rule                        <= Allowed_Use_Decla              and
               Rule                        <= Component_Decla                and
               Rule                        <= Independent_Components_Decla   and
               Rule                        <= Use_Decla                      and
               Rule                        <= Restricted_Use_Decla           and
               Rule                        <= Layer_Decla                    and
               Rule                        <= Forbidden_Use_Decla            and
               Rule                        <= Allowed_Use_Decla              and
               Rule                        <= Component_Decla                and
               Rule                        <= Independent_Components_Decla   and

               Independent_Components_Decla <= Unit_List & Are_T & Independent_T      + Store_Independent_Components'Access and
               Component_Decla       <= Unit & Contains_T & Unit_List                 + Store_Component_Decla'Access        and
               Layer_Decla           <= Unit & Is_T & A_T & Layer_T & Over_T & Unit   + Store_Layer_Decla'Access            and
               Use_Decla             <= Unit & May_T & Use_T & Unit_List              + Store_Use_Decla'Access              and
               Restricted_Use_Decla  <= Only_T & Unit & May_T & Use_T & Unit_List     + Store_Restricted_Use_Decla'Access   and
               Forbidden_Use_Decla   <= Unit & Use_T & Is_T & Forbidden_T             + Add_Forbidden_Unit'Access           and
               Allowed_Use_Decla     <= Unit & Use_T & Is_T & Allowed_T               + Add_Allowed_Unit'Access             and
               Independent_Components_Decla <= Unit_List & Are_T & Independent_T      + Store_Independent_Components'Access and
               Component_Decla       <= Unit & Contains_T & Unit_List                 + Store_Component_Decla'Access        and
               Layer_Decla           <= Unit & Is_T & A_T & Layer_T & Over_T & Unit   + Store_Layer_Decla'Access            and
               Use_Decla             <= Unit & May_T & Use_T & Unit_List              + Store_Use_Decla'Access              and
               Restricted_Use_Decla  <= Only_T & Unit & May_T & Use_T & Unit_List     + Store_Restricted_Use_Decla'Access   and
               Forbidden_Use_Decla   <= Unit & Use_T & Is_T & Forbidden_T             + Add_Forbidden_Unit'Access           and
               Allowed_Use_Decla     <= Unit & Use_T & Is_T & Allowed_T               + Add_Allowed_Unit'Access             and

               Unit_List                   <= Unit & Comma_T & Unit_List     and
               Unit_List                   <= Unit & And_T   & Unit_List     and
               Unit_List                   <= Unit                           and

               Unit                        <= Unit & Dot_T & Identifier_T             + Add_Dot_Identifier'Access     and
               -- Unit                        <= Unit & Dot_T & Star_T                   + Add_Dot_Identifier'Access     and
               Unit                        <= Unit & Star_T                           + Add_Final_Star'Access and
               -- Unit                        <= Star_T                                  + Initialize_Unit_Name'Access and
               Unit                        <= Identifier_T                            + Initialize_Unit_Name'Access;


   -- Step 7: Generating a parser
   -- --------------------------------------------------------------------------

   --  The LALR parser instance.
   Rules_File_Parser : LALR_Parser.Instance := LALR_Parser.Initialize
     (Analyzer,
      Table => LALR_Generator.Generate (Grammar));

   -- --------------------------------------------------------------------------
   Left_List  : Units.Dependency_Targets.List;
   Right_List : Units.Dependency_Targets.List;
   -- To simplify the OpenToken mess, and avoid to create new classes,
   -- Unit_List are stored here, at a global level.
   -- A maximum of two Unit_List is involved in each rule :
   -- unit1 and unit2 may use unit3, unit4 and unit5
   -- ===============         ======================
   --    Left list                  Right list

   -- --------------------------------------------------------------------------
   procedure Reset_Unit_Names is
   begin
      Left_List.Clear;
      Right_List.Clear;
   end Reset_Unit_Names;

   -- --------------------------------------------------------------------------
   Context : Sources.Parsing_Context;

   -- --------------------------------------------------------------------------
   function Current_Location return Sources.Location is
     (File    => +Settings.Rules_File_Name,
      Context => Context,
      Line    => Rules_File_Parser.Line,
      Column  => 0) with Inline;

   -- --------------------------------------------------------------------------
   function Location_Image return String is
     (Location_Image (Current_Location)) with Inline;

   -- --------------------------------------------------------------------------
   Context : Sources.Parsing_Context;

   -- --------------------------------------------------------------------------
   function Current_Location return Sources.Location is
     (File    => +Settings.Rules_File_Name,
      Context => Context,
      Line    => Rules_File_Parser.Line,
      Column  => 0) with Inline;

   -- --------------------------------------------------------------------------
   function Location_Image return String is
     (Location_Image (Current_Location)) with Inline;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize_Unit_Name
   --    Process Identifier_T or Identifier_T + Star_T
   -- --------------------------------------------------------------------------
   procedure Initialize_Unit_Name (New_Token : out Nonterminal.Class;
                                   Source    : in  Token_List.Instance'Class;
                                   To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator :=
                Token_List.Initial_Iterator (Source);

      use OpenToken.Buffers;
      Component_Name : constant Unit_Name
        := +(To_String (Identifiers.Instance
             (Token_List.Token_Handle (Left).all).Identifier));
      Dep : constant Units.Dependency_Target := (To_Unit  => Component_Name,
                                                 Location => Current_Location);
   begin
      Archicheck.IO.Put_Line (Prefix & "Initialize_Unit_Name >" & (+Component_Name) & "<",
                              Level => Archicheck.IO.Verbose);
      if Left_List.Is_Empty then
         Left_List.Append (Dep);
         -- IO.Put_Line (Units.Location_Image (Dep)
         --              & (+Dep.To_Unit) & " added to Left list",
         --             Level => Debug);
      else
         Right_List.Append (Dep);
         -- IO.Put_Line (Units.Location_Image (Dep)
         --              & (+Dep.To_Unit) & " added to Right list",
         --              Level => Debug);
      end if;
   end Initialize_Unit_Name;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dot_Identifier
   -- --------------------------------------------------------------------------
   procedure Add_Dot_Identifier (New_Token : out Nonterminal.Class;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use OpenToken.Buffers;
      -- -----------------------------------------------------------------------
      procedure Update_Last (List : in out Units.Dependency_Targets.List;
                             Name : in     String) is
         Current : constant Units.Dependency_Target := List.Last_Element;
      begin
         List.Replace_Element
           (List.Last, (To_Unit  => Current.To_Unit & "." & Name,
                        Location => Current_Location));
      end Update_Last;

   begin
      Token_List.Next_Token (Right); -- move "Right" over "."
      Token_List.Next_Token (Right); -- move "Right" over the next Identifier

      declare
         Component_Name : constant String := To_String
           (Identifiers.Instance
              (Token_List.Token_Handle (Right).all).Identifier);
         use IO;
      begin
         Put_Line (Prefix & "Add_Dot_Identifier >." & (Component_Name) & "<",
                   Level => Verbose);
         if Right_List.Is_Empty then
            Update_Last (Left_List, Component_Name);

         else
            Update_Last (Right_List, Component_Name);

         end if;

      end;

   end Add_Dot_Identifier;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Final_Star
   -- --------------------------------------------------------------------------
   procedure Add_Final_Star (New_Token : out Nonterminal.Class;
                             Source    : in  Token_List.Instance'Class;
                             To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      -- -----------------------------------------------------------------------
      procedure Update_Last (List : in out Units.Dependency_Targets.List) is
         Current : constant Units.Dependency_Target := List.Last_Element;
      begin
         List.Replace_Element
           (List.Last, (To_Unit  => Current.To_Unit & '*',
                        Location => Current_Location));
      end Update_Last;

   begin
      Token_List.Next_Token (Right); -- move "Right" over "*"

      if Right_List.Is_Empty then
         Archicheck.IO.Put_Line 
           (Item => Prefix & "Add_Final_Star >" & 
              To_String (Left_List.Last_Element.To_Unit) & "*<",
            Level => Archicheck.IO.Verbose);
         Update_Last (Left_List);

      else
         Archicheck.IO.Put_Line (Prefix & "Add_Final_Star >" & 
                                   To_String (Right_List.Last_Element.To_Unit) & "*<",
                                 Level => Archicheck.IO.Verbose);
         Update_Last (Right_List);

      end if;

   end Add_Final_Star;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Component_Decla
   -- Procedure: Store_Component_Decla
   -- -------------------------------------------------------------------------
   procedure Store_Component_Decla
   procedure Store_Component_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      Component_Name : constant Unit_Name := Left_List.First_Element.To_Unit;
      
      
   begin
      if Settings.List_Rules then
         IO.Put_Line (Location_Image
         IO.Put_Line (Location_Image
                      & "Component " & (+Component_Name) & " contains unit "
                      & Units.Unit_List_Image (Right_List),
                      Level => IO.Quiet);
         -- Fixme: put_line to be moved in rules, or in units
         -- Fixme: put_line to be moved in rules, or in units
      end if;
      Units.Add_Component (Component => (Name => Component_Name,
                                         Kind => Units.Component),
                           Targets   => Right_List);
      Reset_Unit_Names;
   end Store_Component_Decla;
   end Store_Component_Decla;

   -- --------------------------------------------------------------------------
   -- Procedure: Store_Layer_Decla
   -- Procedure: Store_Layer_Decla
   -- --------------------------------------------------------------------------
   procedure Store_Layer_Decla (New_Token : out Nonterminal.Class;
   procedure Store_Layer_Decla (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID, Source);

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;
      Used  : constant Unit_Name := Right_List.First_Element.To_Unit;

   begin
      Add_Rule ((Subject_Unit => Using,
                 Object_Unit  => Used,
                 Kind         => Layer_Over,
                 Location     => Current_Location));
      Reset_Unit_Names;

   end Store_Layer_Decla;
   end Store_Layer_Decla;

   -- --------------------------------------------------------------------------
   -- Procedure: Store_Use_Decla
   -- Procedure: Store_Use_Decla
   -- --------------------------------------------------------------------------
   procedure Store_Use_Decla (New_Token : out Nonterminal.Class;
   procedure Store_Use_Decla (New_Token : out Nonterminal.Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      for U of Right_List loop
         Add_Rule ((Subject_Unit => Using,
                    Object_Unit  => U.To_Unit,
                    Kind         => May_Use,
                    Location     => Current_Location));
      end loop;

      Reset_Unit_Names;

   end Store_Use_Decla;
   end Store_Use_Decla;

   -- --------------------------------------------------------------------------
   -- Procedure: Store_Restricted_Use_Decla
   -- Procedure: Store_Restricted_Use_Decla
   -- --------------------------------------------------------------------------
   procedure Store_Restricted_Use_Decla
   procedure Store_Restricted_Use_Decla
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      for U of Right_List loop
         Add_Rule ((Subject_Unit => Using,
                    Object_Unit  => U.To_Unit,
                    Kind         => Exclusive_Use,
                    Location     => Current_Location));
      end loop;

      Reset_Unit_Names;

   end Store_Restricted_Use_Decla;
   end Store_Restricted_Use_Decla;

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit (New_Token : out Nonterminal.Class;
                               Source    : in  Token_List.Instance'Class;
                               To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      Unit : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      Add_Rule ((Kind         => Allowed_Use,
                 Subject_Unit => Unit,
                 Location     => Current_Location));
      Reset_Unit_Names;

   end Add_Allowed_Unit;

   -- --------------------------------------------------------------------------
   procedure Add_Forbidden_Unit (New_Token : out Nonterminal.Class;
   procedure Add_Forbidden_Unit (New_Token : out Nonterminal.Class;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      Unit : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      Add_Rule ((Kind         => Forbidden_Use,
                 Subject_Unit => Unit,
                 Location     => Current_Location));
      Reset_Unit_Names;

   end Add_Forbidden_Unit;
   end Add_Forbidden_Unit;

   -- --------------------------------------------------------------------------
   procedure Store_Independent_Components 
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      use Units.Dependency_Targets;

      I, J : Units.Dependency_Targets.Cursor;

   begin
      -- For the "A, B, C are independent" rule, Left_List will contains A,
      -- and Right B, C, etc.
      -- So, lets merge both
      Right_List.Prepend (Left_List.First_Element);
      -- Put_Debug_Line ("Right_List = " & Right_List'Image);
      -- To check the "A, B, C are independent" rule, we have 
      -- to register all possible couple : AB, AC, BC
      I := Right_List.First;
      loop
         exit when I = Right_List.Last;
         J := Next (I);
         loop
            Add_Rule ((Subject_Unit => Right_List (I).To_Unit,
                       Object_Unit  => Right_List (J).To_Unit,
                       Kind         => Are_Independent,
                       Location     => Current_Location));
            exit when J = Right_List.Last;
            Next (J);
         end loop;
         Next (I);
      end loop;

      Reset_Unit_Names;

   end Store_Independent_Components;

   -- -------------------------------------------------------------------------
   -- Procedure: Parse
   -- -------------------------------------------------------------------------
   procedure Parse (Rules_File : in File_Name) is
      use Ada.Text_IO;

      procedure Parse is 
      begin
         Analyzer.Reset;
         Analyzer.Set_Text_Feeder (OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Current_Input));
         LALR_Parser.Parse (Rules_File_Parser);
      exception
         when Error : others =>
            IO.Put_Error (Location_Image & "parse exception");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
      end Parse;

      procedure Parse is 
      begin
         Analyzer.Reset;
         Analyzer.Set_Text_Feeder (OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Current_Input));
         LALR_Parser.Parse (Rules_File_Parser);
      exception
         when Error : others =>
            IO.Put_Error (Location_Image & "parse exception");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
      end Parse;

   begin
      --  if Settings.Debug_Mode then
      --     OpenToken.Trace_Parse := 1;  -- value > 0 : debug level
      --  end if;

      -- Analyze first the rules file
      Context := Sources.In_File;
      Open (Input_File,
            Mode => In_File,
            Name => +Rules_File);
      Set_Input (Input_File);
      Parse;
      Parse;
      Close (Input_File);

      -- and then command line rules
      if Is_Open (Settings.Cmd_Line_Rules_File) then
         Context := In_Command_Line;
         Reset (Settings.Cmd_Line_Rules_File,
                Mode => In_File);
         Set_Input (Settings.Cmd_Line_Rules_File);
         Parse;
         Close (Settings.Cmd_Line_Rules_File);
      end if;

      -- and then command line rules
      if Is_Open (Settings.Cmd_Line_Rules_File) then
         Context := In_Command_Line;
         Reset (Settings.Cmd_Line_Rules_File,
                Mode => In_File);
         Set_Input (Settings.Cmd_Line_Rules_File);
         Parse;
         Close (Settings.Cmd_Line_Rules_File);
      end if;

   end Parse;

end Archicheck.Rules.Parser;
