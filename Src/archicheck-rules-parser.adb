-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
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

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- --------------------------------------------------------------------------
--     procedure Put_Debug_Line (Msg    : in String  := "";
--                               Debug  : in Boolean := Settings.Debug_Mode; --True; -- change to True to debug this package
--                               Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug (Msg    : in String  := "";
   --                          Debug  : in Boolean := True;
   --                          Prefix : in String  := "Rules_Parser") renames Archicheck.IO.Put_Debug;


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
      Contains_Id,
      Is_Id,
      Layer_Id,
      Only_Id,
      May_Id,
      Over_Id,
      Use_Id,
      Forbidden_Id,
      Allowed_Id,

      -- Misc
      Comma_Id, -- ','
      Dot_Id, -- '.'
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
      Component_Declaration_Id,
      Layer_Declaration_Id,
      Use_Declaration_Id,
      Restricted_Use_Declaration_Id,
      Forbidden_Use_Declaration_Id,
      Allowed_Used_Declaration_Id
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
              (Whitespace_Id    => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
               --
               A_Id             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("a")),
               And_Id           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
               Contains_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("contains")),
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
               Semicolon_Id     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),

               EoF_Id           => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),

               --  Identifier must be after keywords, so they are recognized instead
               Identifier_Id    => Tokenizer.Get
                 (Recognizer => OpenToken.Recognizer.Identifier.Get
                    (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
                     Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
                  New_Token  => Identifiers.Get (Identifier_Id))

               -- EoL_Id        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ((1 => OpenToken.EOL_Character))),
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
   Layer_T      : constant Master_Token.Class := Master_Token.Get (Layer_Id);
   Only_T       : constant Master_Token.Class := Master_Token.Get (Only_Id);
   May_T        : constant Master_Token.Class := Master_Token.Get (May_Id);
   Use_T        : constant Master_Token.Class := Master_Token.Get (Use_Id);
   Forbidden_T  : constant Master_Token.Class := Master_Token.Get (Forbidden_Id);
   Allowed_T    : constant Master_Token.Class := Master_Token.Get (Allowed_Id);
   Over_T       : constant Master_Token.Class := Master_Token.Get (Over_Id);
   Semicolon_T  : constant Master_Token.Class := Master_Token.Get (Semicolon_Id);

   -- Non-terminal tokens, which define the grammar.
   Use_Declaration            : constant Nonterminal.Class := Nonterminal.Get (Use_Declaration_Id);
   Restricted_Use_Declaration : constant Nonterminal.Class := Nonterminal.Get (Restricted_Use_Declaration_Id);
   Forbidden_Use_Declaration  : constant Nonterminal.Class := Nonterminal.Get (Forbidden_Use_Declaration_Id);
   Allowed_Use_Declaration    : constant Nonterminal.Class := Nonterminal.Get (Allowed_Used_Declaration_Id);
   Layer_Declaration          : constant Nonterminal.Class := Nonterminal.Get (Layer_Declaration_Id);
   Component_Declaration      : constant Nonterminal.Class := Nonterminal.Get (Component_Declaration_Id);
   Rule                       : constant Nonterminal.Class := Nonterminal.Get (Rule_Id);
   Rule_List                  : constant Nonterminal.Class := Nonterminal.Get (Rule_List_Id);
   Rules_File                 : constant Nonterminal.Class := Nonterminal.Get (Rules_File_Id);
   Unit                       : constant Nonterminal.Class := Nonterminal.Get (Unit_Id);
   Unit_List                  : constant Nonterminal.Class := Nonterminal.Get (Unit_List_Id);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;


   -- --------------------------------------------------------------------------
   procedure Initialize_Unit_Name (New_Token : out Nonterminal.Class;
                                   Source    : in  Token_List.Instance'Class;
                                   To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_To_Unit_Name (New_Token : out Nonterminal.Class;
                               Source    : in  Token_List.Instance'Class;
                               To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Use_Declaration (New_Token : out Nonterminal.Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Store_Restricted_Use_Declaration (New_Token : out Nonterminal.Class;
                                                Source    : in  Token_List.Instance'Class;
                                                To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit (New_Token : out Nonterminal.Class;
                               Source    : in  Token_List.Instance'Class;
                               To_ID     : in  Master_Token.Token_ID);

   -- --------------------------------------------------------------------------
   procedure Add_Forbbiden_Unit (New_Token : out Nonterminal.Class;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  Master_Token.Token_ID);


   -- Grammar:
   -- --------------------------------------------------------------------------
   -- Rules_File                  -> Rule_List & EoF
   -- Rule_List                   -> Rule [Rule]*
   -- Separator                   -> , | and | blank
   -- Unit                        -> Identifier[.Identifier]*
   -- Unit_List                   -> Unit [[Separator]* Unit]*                    Unit_List = Unit & Unit & Unit etc.
   -- Component_Declaration       -> Component contains Unit_List                 Insert [Component, Unit_List] in Component_List
   -- Layer_Declaration           -> Layer is a layer over Layer                  Insert [Layer, Layer]         in Layer_List
   -- Restricted_Use_Declaration -> only Component1 may use component2
   -- Use_Declaration             -> Component1 uses Component2
   -- Rule                        -> Component_Declaration | Layer_Declaration | Use_Declaration | Restricted_Use_Declaration

   Grammar : constant Production_List.Instance :=
               Rules_File                  <= Rule_List & EoF_T              and

               Rule_List                   <= Rule_List & Semicolon_T        and
               Rule_List                   <= Rule_List & Rule               and
               Rule_List                   <= Rule                           and

               Rule                        <= Use_Declaration                and
               Rule                        <= Restricted_Use_Declaration     and
               Rule                        <= Layer_Declaration              and
               Rule                        <= Forbidden_Use_Declaration      and
               Rule                        <= Allowed_Use_Declaration        and
               Rule                        <= Component_Declaration          and

               Component_Declaration       <= Unit & Contains_T & Unit_List                 + Store_Component_Declaration'Access      and
               Layer_Declaration           <= Unit & Is_T & A_T & Layer_T & Over_T & Unit   + Store_Layer_Declaration'Access          and
               Use_Declaration             <= Unit & May_T & Use_T & Unit_List              + Store_Use_Declaration'Access            and
               Restricted_Use_Declaration  <= Only_T & Unit & May_T & Use_T & Unit_List     + Store_Restricted_Use_Declaration'Access and
               Forbidden_Use_Declaration   <= Unit & Use_T & Is_T & Forbidden_T             + Add_Forbbiden_Unit'Access               and
               Allowed_Use_Declaration     <= Unit & Use_T & Is_T & Allowed_T               + Add_Allowed_Unit'Access                 and

               Unit_List                   <= Unit & Comma_T & Unit_List     and
               Unit_List                   <= Unit & And_T   & Unit_List     and
               Unit_List                   <= Unit                           and

               Unit                        <= Unit & Dot_T & Identifier_T                   + Add_To_Unit_Name'Access                 and
               Unit                        <= Identifier_T                                  + Initialize_Unit_Name'Access;

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
                  OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer   : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  The LALR parser instance.
   Rules_File_Parser : LALR_Parser.Instance := LALR_Parser.Initialize
     (Analyzer,
      LALR_Generator.Generate (Grammar)); -- , Ignore_Unused_Tokens => True));

   -- --------------------------------------------------------------------------
   -- Fixme: Fonctionnement Très Spécial, À Commenter!
   Left_List  : Units.Dependency_Targets.List;
   Right_List : Units.Dependency_Targets.List;
   -- To simplify the OpenToken mess, and avoid to creating new classes,
   -- Unit_List are stored here, at a global level.
   -- A maximum of two Unit_List is involved in each rule :
   -- unit1 and unit2 may use unit3 and unit4
   -- ===============         ===============
   --    Left list              Right list


   -- --------------------------------------------------------------------------
   procedure Reset_Unit_Names is
   begin
      Left_List.Clear;
      Right_List.Clear;
   end Reset_Unit_Names;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize_Unit_Name
   -- --------------------------------------------------------------------------
   procedure Initialize_Unit_Name (New_Token : out Nonterminal.Class;
                                   Source    : in  Token_List.Instance'Class;
                                   To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Left  : constant Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;
      Component_Name : constant Unit_Name
        := +(To_String (Identifiers.Instance (Token_List.Token_Handle (Left).all).Identifier));
      Dep            : constant Units.Dependency_Target :=
                         (To_Unit => Component_Name,
                          File    => +Settings.Rules_File_Name, -- Source_Name'(To_Unbounded_String (Settings.Rules_File_Name)),
                          Line    => Rules_File_Parser.Line);
   begin
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
   -- Procedure: Add_To_Unit_Name
   -- --------------------------------------------------------------------------
   procedure Add_To_Unit_Name (New_Token : out Nonterminal.Class;
                               Source    : in  Token_List.Instance'Class;
                               To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);

      use Archicheck.IO;
      use OpenToken.Buffers;
      -- -----------------------------------------------------------------------
      procedure Update_Last (List : in out Units.Dependency_Targets.List;
                             Name : in     String) is
         Current : constant Units.Dependency_Target := List.Last_Element;
      begin
         List.Replace_Element
           (List.Last, (To_Unit => Current.To_Unit & "." & Name,
                        File    => Current.File,
                        Line    => Current.Line));
      end Update_Last;

   begin
      Token_List.Next_Token (Right); -- move "Right" over "."
      Token_List.Next_Token (Right); -- move "Right" over the next Identifier

      declare
         Component_Name : constant String := To_String
           (Identifiers.Instance (Token_List.Token_Handle (Right).all).Identifier);
      begin
         if Right_List.Is_Empty then
            -- Put_Line (Prefix & "Unit_Name_A = " & (+Unit_Name_A));
            Update_Last (Left_List, Component_Name);

         else
            -- Put_Line (Prefix & "Unit_Name_B = " & (+Unit_Name_B));
            Update_Last (Right_List, Component_Name);

         end if;

      end;

   end Add_To_Unit_Name;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Component_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Component_Declaration (New_Token : out Nonterminal.Class;
                                          Source    : in  Token_List.Instance'Class;
                                          To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      use OpenToken.Buffers;
      Component_Name : constant Unit_Name := Left_List.First_Element.To_Unit;
   begin
      if Settings.List_Rules then
         IO.Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                                  Line   => Rules_File_Parser.Line)
                      & "Component " & (+Component_Name) & " contains unit "
                      & Units.Unit_List_Image (Right_List),
                      Level => IO.Quiet);
      end if;
      Units.Add_Component
        (Component => (Name => Component_Name,
                       Kind => Units.Component),
         Targets   => Right_List);
      Reset_Unit_Names;
   end Store_Component_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Layer_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Layer_Declaration (New_Token : out Nonterminal.Class;
                                      Source    : in  Token_List.Instance'Class;
                                      To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, To_ID, Source);

      use Archicheck.IO;
      use OpenToken.Buffers;

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;
      Used  : constant Unit_Name := Right_List.First_Element.To_Unit;

   begin
      if Settings.List_Rules then
         Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                               Line   => Analyzer.Line)
                   & "Layer " & (+Using)
                   & " is over layer " & (+Used),
                   Level => IO.Quiet);
      end if;
      Add_Rule ((Subject_Unit => Using,
                 Object_Unit  => Used,
                 Kind         => Layer_Over));
      Reset_Unit_Names;

   end Store_Layer_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Use_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Use_Declaration (New_Token : out Nonterminal.Class;
                                    Source    : in  Token_List.Instance'Class;
                                    To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      use Archicheck.IO;
      use OpenToken.Buffers;

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      if Settings.List_Rules then
         Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                               Line   => Analyzer.Line)
                   & (+Using) & " may use " -- Used,
                   & Units.Unit_List_Image (Right_List),
                   Level => IO.Quiet);
      end if;
      for U of Right_List loop
         Add_Rule ((Subject_Unit => Using,
                    Object_Unit  => U.To_Unit,
                    Kind         => May_Use));
      end loop;

      Reset_Unit_Names;

   end Store_Use_Declaration;

   -- -------------------------------------------------------------------------
   -- Procedure: Store_Restricted_Use_Declaration
   -- -------------------------------------------------------------------------
   procedure Store_Restricted_Use_Declaration (New_Token : out Nonterminal.Class;
                                               Source    : in  Token_List.Instance'Class;
                                               To_ID     : in  Master_Token.Token_ID)
   is
      pragma Unreferenced (New_Token, Source, To_ID);

      use Archicheck.IO;
      use OpenToken.Buffers;

      Using : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      if Settings.List_Rules then
         Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                               Line   => Analyzer.Line)
                   & "Only " & (+Using) & " may use "
                   & Units.Unit_List_Image (Right_List),
                   Level => IO.Quiet);
      end if;
      for U of Right_List loop
         Add_Rule ((Subject_Unit => Using,
                    Object_Unit  => U.To_Unit,
                    Kind         => Exclusive_Use));
      end loop;

      Reset_Unit_Names;

   end Store_Restricted_Use_Declaration;

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit (New_Token : out Nonterminal.Class;
                               Source    : in  Token_List.Instance'Class;
                               To_ID     : in  Master_Token.Token_ID) is
      pragma Unreferenced (New_Token, Source, To_ID);

      use Archicheck.IO;
      use OpenToken.Buffers;
      Unit : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      if Settings.List_Rules then
         Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                               Line   => Analyzer.Line) &
                     "Use of " & (+Unit) & " allowed ",
                   Level => IO.Quiet);
      end if;
      Add_Rule ((Kind         => Allowed_Use,
                 Subject_Unit => Unit));
      Reset_Unit_Names;

   end Add_Allowed_Unit;

   -- --------------------------------------------------------------------------
   procedure Add_Forbbiden_Unit (New_Token : out Nonterminal.Class;
                                 Source    : in  Token_List.Instance'Class;
                                 To_ID     : in  Master_Token.Token_ID) is
      pragma Unreferenced (New_Token, Source, To_ID);

      use Archicheck.IO;
      use OpenToken.Buffers;
      Unit : constant Unit_Name := Left_List.First_Element.To_Unit;

   begin
      if Settings.List_Rules then
         Put_Line (GNU_Prefix (File   => +Settings.Rules_File_Name,
                               Line   => Analyzer.Line) &
                     "Use of " & (+Unit) & " is forbidden",
                   Level => IO.Quiet);
      end if;
      Add_Rule ((Kind         => Forbidden_Use,
                 Subject_Unit => Unit));
      Reset_Unit_Names;

   end Add_Forbbiden_Unit;


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
            IO.Put_Error (Sources.GNU_Prefix (+File_Name, Analyzer.Line)
                          & "parse exception");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));

      end Parsing;

      Close (Input_File);

   end Parse;

end Archicheck.Rules.Parser;
