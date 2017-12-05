-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Rules_Parser body
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
-- with OpenToken.Recognizer.Line_Comment;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Recognizer.Keyword;

with Archicheck.IO;
with Archicheck.Settings;
with Archicheck.Components;   use Archicheck.Components;
with Archicheck.Layers;

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

package body Archicheck.Rules_Parser is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False; -- change to True to debug this package
                             Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug (Msg    : in String  := "";
   --                          Debug  : in Boolean := True;
   --                          Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug;


   -- The list of tokens, without non-terminals in recursive descent.
   type Token_Ids is
     (-- Non reporting tokens (not used in generating an LALR grammar) -----------------------------------------
      Whitespace_Id,
      -- Comment_Id,
      --        EoL_Id,
      --        Minus_Id,

      -- Terminals tokens ---------------
      -- Keywords
      A_Id, -- first terminal
      And_Id,
      Contains_Id,
      Is_Id,
      Layer_Id,
      Only_Id,
      May_Id,
      Over_Id,
      Use_Id,

      -- Misc
      Comma_Id, -- ,
      Dot_Id, -- .
      Semicolon_Id, -- ;
      EoF_Id,
      -- Whitespace_Id,
      -- Comment_Id,
      --        EoL_Id,
      --        Minus_Id,
      --  Identifier must be after keywords, so they are recognized instead
      Identifier_Id, -- last terminal

      -- Non-terminals tokens ---------------
      Rules_File_Id,
      Rule_List_Id,
      Rule_Id,
      -- Unit_Id,
      Unit_List_Id,
      Component_Declaration_Id,
      Layer_Declaration_Id,
      Use_Declaration_Id,
      Use_Restriction_Declaration_Id
     );
   First_Terminal : constant Token_Ids := A_Id;
   Last_Terminal  : constant Token_Ids := Identifier_Id;

   -- private

   package Master_Token is new OpenToken.Token.Enumerated
     (Token_Ids, First_Terminal, Last_Terminal, Token_Ids'Image);
   package Tokenizer is new Master_Token.Analyzer;

   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Identifiers is new Master_Token.Identifier;
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
   Syntax : constant Tokenizer.Syntax :=
              (Whitespace_Id => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
               -- Comment_Id    => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--", Reportable => True)), --** que veut dire le reportable??
               --
               A_Id          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a")),
               And_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
               Contains_Id   => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("contains")),
               Is_Id         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is")),
               Layer_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("layer")),
               Only_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("only")),
               May_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("may")),
               Over_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("over")),
               Use_Id        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use")),

               -- Delimiters
               -- Comment_Id    => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--", Reportable => True)), --** que veut dire le reportable??
               Comma_Id      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
               -- Whitespace_Id => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
               Dot_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
               -- Minus_Id      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-")),
               Semicolon_Id  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

               EoF_Id        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),

               --  Identifier must be after keywords, so they are recognized instead
               Identifier_Id => Tokenizer.Get
                 (Recognizer => OpenToken.Recognizer.Identifier.Get
                    (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
                     Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
                  New_Token  => Identifiers.Get (Identifier_Id))
               --
               -- EoL_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ((1 => OpenToken.EOL_Character))),
               -- Bad_Token_Id  => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
                );

   --  Terminal tokens
   A_T          : constant Master_Token.Class := Master_Token.Get (A_Id);
   And_T        : constant Master_Token.Class := Master_Token.Get (And_Id);
   Contains_T   : constant Master_Token.Class := Master_Token.Get (Contains_Id);
   Comma_T      : constant Master_Token.Class := Master_Token.Get (Comma_Id);
   Dot_T        : constant Master_Token.Class := Master_Token.Get (Dot_Id);
   Identifier_T : constant Master_Token.Class := Master_Token.Get (Identifier_Id);
   Is_T         : constant Master_Token.Class := Master_Token.Get (Is_Id);
   EoF_T        : constant Master_Token.Class := Master_Token.Get (EoF_Id);
   -- EoL       : constant Master_Token.Class := Master_Token.Get (EoL_Id);
   Layer_T      : constant Master_Token.Class := Master_Token.Get (Layer_Id);
   Only_T       : constant Master_Token.Class := Master_Token.Get (Only_Id);
   May_T        : constant Master_Token.Class := Master_Token.Get (May_Id);
   Use_T        : constant Master_Token.Class := Master_Token.Get (Use_Id);
   Over_T       : constant Master_Token.Class := Master_Token.Get (Over_Id);
   Semicolon_T  : constant Master_Token.Class := Master_Token.Get (Semicolon_Id);
   -- Comment_T   : constant Master_Token.Class := Master_Token.Get (Comment_Id);

   -- Non-terminal tokens, which define the grammar.
   Use_Declaration             : constant Nonterminal.Class := Nonterminal.Get (Use_Declaration_Id);
   Use_Restriction_Declaration : constant Nonterminal.Class := Nonterminal.Get (Use_Restriction_Declaration_Id);
   Layer_Declaration           : constant Nonterminal.Class := Nonterminal.Get (Layer_Declaration_Id);
   Component_Declaration       : constant Nonterminal.Class := Nonterminal.Get (Component_Declaration_Id);
   Rule                        : constant Nonterminal.Class := Nonterminal.Get (Rule_Id);
   Rule_List                   : constant Nonterminal.Class := Nonterminal.Get (Rule_List_Id);
   Rules_File                  : constant Nonterminal.Class := Nonterminal.Get (Rules_File_Id);
   -- Unit                        : constant Nonterminal.Class := Nonterminal.Get (Unit_Id);
   Unit_List                   : constant Nonterminal.Class := Nonterminal.Get (Unit_List_Id);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;


--     -- -------------------------------------------------------------------------
--     procedure Initialize_Unit_Name (New_Token : out Nonterminal.Class;
--                                     Source    : in  Token_List.Instance'Class;
--                                     To_ID     : in  Master_Token.Token_ID);
--
--     -- -------------------------------------------------------------------------
--     procedure Store_Unit_Name (New_Token : out Nonterminal.Class;
--                                Source    : in  Token_List.Instance'Class;
--                                To_ID     : in  Master_Token.Token_ID);
--
   -- -------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Store_Use_Declaration (New_Token : out Nonterminal.Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Store_Use_Restriction_Declaration (New_Token : out Nonterminal.Class;
                                                Source    : in  Token_List.Instance'Class;
                                                To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Initialize_Unit_List (New_Token : out Nonterminal.Class;
                                   Source    : in  Token_List.Instance'Class;
                                   To_ID     : in  Master_Token.Token_ID);

   -- -------------------------------------------------------------------------
   procedure Append_Unit_To_List (New_Token : out Nonterminal.Class;
                                  Source    : in  Token_List.Instance'Class;
                                  To_ID     : in  Master_Token.Token_ID);

   -- Grammar:
   --
   -- Rules_File                  -> Rule_List & EoF
   -- Rule_List                   -> Rule [Rule]*
   -- Separator                   -> , | and | blank
   -- Unit                        -> Identifier[.Identifier]*
   -- Unit_List                   -> Unit [[Separator]* Unit]*                    Unit_List = Unit & Unit & Unit etc.
   -- Component_Declaration       -> Component contains Unit_List                 Insert [Component, Unit_List] in Component_List
   -- Layer_Declaration           -> Layer is a layer over Layer                  Insert [Layer, Layer]         in Layer_List
   -- Use_Restriction_Declaration -> only Component1 may use component2
   -- Use_Declaration             -> Component1 uses Component2
   -- Rule                        -> Component_Declaration | Layer_Declaration | Use_Declaration | Use_Restriction_Declaration

   Grammar : constant Production_List.Instance :=
               Rules_File                  <= Rule_List & EoF_T                  and
               Rule_List                   <= Rule_List & Semicolon_T            and
               Rule_List                   <= Rule_List & Dot_T                  and
               Rule_List                   <= Rule_List & Rule                   and
               Rule_List                   <= Rule                               and
               -- Rule                   <= Comment_T                          and
               Rule                        <= Use_Declaration                    and
               Rule                        <= Use_Restriction_Declaration        and
               Rule                        <= Layer_Declaration                  and
               Rule                        <= Component_Declaration              and
--                 Unit                        <= Unit & Dot_T & Identifier_T     + Store_Unit_Name'Access      and
--                 Unit                        <= Identifier_T                    + Initialize_Unit_Name'Access and
               Unit_List                   <= Identifier_T & Comma_T & Unit_List                          + Append_Unit_To_List'Access         and
               Unit_List                   <= Identifier_T & And_T   & Unit_List                          + Append_Unit_To_List'Access         and
               -- Unit_List              <= Identifier           & Unit_List                    + Append_Unit_To_List'Access         and
               Unit_List                   <= Identifier_T                                                + Initialize_Unit_List'Access        and
--                Unit_List                   <= Unit & Comma_T & Unit_List                          + Append_Unit_To_List'Access         and
--                Unit_List                   <= Unit & And_T   & Unit_List                          + Append_Unit_To_List'Access         and
--                Unit_List                   <= Unit                                                + Initialize_Unit_List'Access        and
              Component_Declaration       <= Identifier_T & Contains_T & Unit_List                       + Store_Component_Declaration'Access and
              Layer_Declaration           <= Identifier_T & Is_T & A_T & Layer_T & Over_T & Identifier_T + Store_Layer_Declaration'Access     and
              Use_Declaration             <= Identifier_T & Use_T & Identifier_T                         + Store_Use_Declaration'Access       and
              Use_Restriction_Declaration <= Only_T & Identifier_T & May_T & Use_T & Identifier_T        + Store_Use_Restriction_Declaration'Access;

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
                  OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer   : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  The lalr parser instance.
   Rules_File_Parser : LALR_Parser.Instance := LALR_Parser.Initialize
     (Analyzer,
      LALR_Generator.Generate (Grammar, Ignore_Unused_Tokens => True)); --**
   Units             : Unit_Lists.List;
   -- Unit_Name         : Ada.Strings.Unbounded.Unbounded_String;

--     -- -------------------------------------------------------------------------
--     -- Procedure: Initialize_Unit_Name
--     -- -------------------------------------------------------------------------
--     procedure Initialize_Unit_Name (New_Token : out Nonterminal.Class;
--                                     Source    : in  Token_List.Instance'Class;
--                                     To_ID     : in  Master_Token.Token_ID)
--     is
--        pragma Unreferenced (New_Token, To_ID);
--        Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
--
--        use Archicheck.IO;
--        use OpenToken.Buffers;
--        use Ada.Strings.Unbounded;
--        Prefix         : constant String := GNU_Prefix (File   => Settings.Rules_File_Name,
--                                                        Line   => Rules_File_Parser.Line,
--                                                        Column => Rules_File_Parser.Column);
--
--     begin
--        declare
--           Component_Name : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
--        begin
--           Unit_Name := To_Unbounded_String (Component_Name);
--           Put_Line (Prefix & "init Unit_Name = " & To_String (Unit_Name));
--        end;
--
--     end Initialize_Unit_Name;
--
--     -- -------------------------------------------------------------------------
--     -- Procedure: Store_Unit_Name
--     -- -------------------------------------------------------------------------
--     procedure Store_Unit_Name (New_Token : out Nonterminal.Class;
--                                Source    : in  Token_List.Instance'Class;
--                                To_ID     : in  Master_Token.Token_ID)
--     is
--        pragma Unreferenced (New_Token, To_ID);
--        -- Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
--        Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
--
--        use Archicheck.IO;
--        use OpenToken.Buffers;
--        use Ada.Strings.Unbounded;
--        Prefix         : constant String := GNU_Prefix (File   => Settings.Rules_File_Name,
--                                                        Line   => Rules_File_Parser.Line,
--                                                        Column => Rules_File_Parser.Column);
--
--     begin
--        Token_List.Next_Token (Right); -- move "Right" over "."
--        Token_List.Next_Token (Right); -- move "Right" over the next Identifier
--
--        declare
--           Component_Name : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);
--        begin
--           Unit_Name := Unit_Name & '.' & Component_Name;
--           Put_Line (Prefix & "Unit_Name = " & To_String (Unit_Name));
--        end;
--
--     end Store_Unit_Name;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Component_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      -- Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;
      use Ada.Strings.Unbounded;

   begin
      -- Token_List.Next_Token (Right); -- move "Right" over "contains"
      -- Token_List.Next_Token (Right); -- move "Right" over the Unit

      --** Token_List.Print (Source);
      --** New_Line;

      declare
         Component_Name : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
         -- Unit_Name      : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);
         Cursor         : Component_Maps.Cursor;
         Prefix         : constant String := GNU_Prefix (File   => Settings.Rules_File_Name,
                                                         Line   => Rules_File_Parser.Line,
                                                         Column => Rules_File_Parser.Column);
      begin
         if Settings.List_Rules then
            for U of Units loop
               Put_Line (Prefix & "Component " & Component_Name & " contains Unit " & To_String (U));
            end loop;
            -- Put_Line (Prefix & "Component " & Component_Name & " contains Unit " & Unit_List_Image (Units));
         end if;

         Cursor := Component_Maps.Find (Component_Map, Component_Name);

         if Component_Maps.Has_Element (Cursor) then
            -- the component was described by one or more declarations
            -- Units := Component_Maps.Element (Cursor);
            -- Unit_Lists.Append (Units, To_Unbounded_String (Unit_Name));
            Component_Maps.Replace_Element (Component_Map, Cursor, Units);
            Put_Debug_Line ("Adding units " & Unit_List_Image (Units) & " to component " & Component_Name);

         else
            -- First occurence of this component
            -- Unit_Lists.Append (Units, To_Unbounded_String (Unit_Name));
            Component_Maps.Insert (Container => Component_Map,
                                   Key       => Component_Name,
                                   New_Item  => Units);
            Put_Debug_Line ("Creating component " & Component_Name & ", adding units " & Unit_List_Image (Units));
         end if;

      end;

   end Store_Component_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Layer_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;

   begin
      Token_List.Next_Token (Right); -- move "Right" over "is"
      Token_List.Next_Token (Right); -- move "Right" over "a"
      Token_List.Next_Token (Right); -- move "Right" over "layer"
      Token_List.Next_Token (Right); -- move "Right" over "over"
      Token_List.Next_Token (Right); -- move "Right" over the layer

      declare
         use Ada.Strings.Unbounded;
         Using : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
         Used  : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);

      begin
         if Settings.List_Rules then
            Put_Line (GNU_Prefix (File   => Settings.Rules_File_Name,
                                  Line   => Analyzer.Line,
                                  Column => Analyzer.Column)
                      & "Layer " & Using & " is over layer " & Used);
         end if;
         Layers.Add_Layer ((Using_Layer => To_Unbounded_String (Using),
                            Used_Layer  => To_Unbounded_String (Used)));
      end;

   end Store_Layer_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Use_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Use_Declaration (New_Token : out Nonterminal.Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right :          Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;

   begin
      Token_List.Next_Token (Right); -- move "Right" over "use"
      Token_List.Next_Token (Right); -- move "Right" over "component2"

      declare
         use Ada.Strings.Unbounded;
         Using : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
         Used  : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);

      begin
         if Settings.List_Rules then
            Put_Line (GNU_Prefix (File   => Settings.Rules_File_Name,
                                  Line   => Analyzer.Line,
                                  Column => Analyzer.Column)
                      & "Component " & Using & " uses component " & Used);
         end if;
--           Layers.Add_Layer ((Using_Layer => To_Unbounded_String (Using),
--                              Used_Layer  => To_Unbounded_String (Used)));
      end;

   end Store_Use_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Use_Restriction_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Use_Restriction_Declaration (New_Token : out Nonterminal.Class;
                                                Source    : in  Token_List.Instance'Class;
                                                To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;

   begin
      Token_List.Next_Token (Left); -- move "Left" over "component1"

      Token_List.Next_Token (Right); -- move "Right" over "component1"
      Token_List.Next_Token (Right); -- move "Right" over "may"
      Token_List.Next_Token (Right); -- move "Right" over "use"
      Token_List.Next_Token (Right); -- move "Right" over "compoenent 2"

      declare
         use Ada.Strings.Unbounded;
         Using : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
         Used  : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);

      begin
         if Settings.List_Rules then
            Put_Line (GNU_Prefix (File   => Settings.Rules_File_Name,
                                  Line   => Analyzer.Line,
                                  Column => Analyzer.Column)
                      & "Component " & Using & " may use component " & Used);
         end if;
--           Layers.Add_Layer ((Using_Layer => To_Unbounded_String (Using),
--                              Used_Layer  => To_Unbounded_String (Used)));
      end;

   end Store_Use_Restriction_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Append_Unit_To_List
   -- -------------------------------------------------------------------------
   procedure Initialize_Unit_List (New_Token : out Nonterminal.Class;
                                   Source    : in  Token_List.Instance'Class;
                                   To_ID     : in  Master_Token.Token_ID) is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;
      use Ada.Strings.Unbounded;
      Unit_Name : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
      Prefix    : constant String := GNU_Prefix (File   => Settings.Rules_File_Name,
                                                 Line   => Rules_File_Parser.Line,
                                                 Column => Rules_File_Parser.Column);
   begin
      Units := Unit_Lists.Empty_List;
      Unit_Lists.Append (Units, To_Unbounded_String (Unit_Name));
      Put_Debug_Line (Prefix & "Initialize_Unit_List with " & Unit_Name);
   end Initialize_Unit_List;

   -- -------------------------------------------------------------------------
   -- Procedure: Append_Unit_To_List
   -- -------------------------------------------------------------------------
   procedure Append_Unit_To_List (New_Token : out Nonterminal.Class;
                                  Source    : in  Token_List.Instance'Class;
                                  To_ID     : in  Master_Token.Token_ID) is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;
      use Ada.Strings.Unbounded;
      Unit_Name : constant String := To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier);
      Prefix    : constant String := GNU_Prefix (File   => Settings.Rules_File_Name,
                                                 Line   => Rules_File_Parser.Line,
                                                 Column => Rules_File_Parser.Column);
   begin
      Unit_Lists.Append (Units, To_Unbounded_String (Unit_Name));
      Put_Debug_Line (Prefix & "Append_Unit " & Unit_Name & " To_List");
   end Append_Unit_To_List;

   -- -------------------------------------------------------------------------
   -- Procedure: Parse
   -- -------------------------------------------------------------------------
   procedure Parse (File_Name  : in String) is
      use Ada.Text_IO;

   begin
      Open (Input_File,
            Mode => In_File,
            Name => File_Name);
      Set_Input (Input_File);

      if Settings.Debug_Mode then
         OpenToken.Trace_Parse := 1;  -- value > 0 : debug level
      end if;

      Parsing : begin
         LALR_Parser.Parse (Rules_File_Parser);

      exception
         when Error : others =>
            IO.Put_Error (IO.GNU_Prefix (File_Name, Analyzer.Line, Analyzer.Column) & "parse exception");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));

      end Parsing;

      Close (Input_File);

   end Parse;

end Archicheck.Rules_Parser;
