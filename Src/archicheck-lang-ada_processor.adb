-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang.Ada_Processor body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--   - More more thorough Ada implementation needed
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Settings;
with Archicheck.Units;

with Ada_Lexer;

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Archicheck.Lang.Ada_Processor is

   -- --------------------------------------------------------------------------
   Processor : aliased Ada_Interface;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Language_Processor => Processor'Access,
                For_Language       => Sources.Ada_2012);
   end Initialize;

   -- --------------------------------------------------------------------------
   -- Function: File_Extensions
   -- --------------------------------------------------------------------------
   function File_Extensions (Lang : in Ada_Interface) return String is
      pragma Unreferenced (Lang);
   begin
      return Settings.Ada_Files_Pattern;
   end File_Extensions;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   --
   -- Implementation Notes:
   --   - Based on OpenToken Ada_Lexer
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies (Lang        : in Ada_Interface;
                                   From_Source : in String) is
      pragma Unreferenced (Lang);

      -- Change default Debug parameter value to enable/disable Debug messages in this package
      -- -----------------------------------------------------------------------
      procedure Put_Debug_Line (Msg    : in String  := "";
                                Debug  : in Boolean := Settings.Debug_Mode;
                                Prefix : in String  := "Analyze_Dep") renames Archicheck.IO.Put_Debug_Line;
      --        procedure Put_Debug (Msg    : in String  := "";
      --                             Debug  : in Boolean := Settings.Verbosity = Settings.Debug;
      --                             Prefix : in String  := "Ada_Processor.Analyze_Dependencies") renames Archicheck.IO.Put_Debug;
      -- procedure New_Debug_Line (Debug  : in Boolean := Settings.Verbosity = Settings.Debug) renames Archicheck.IO.New_Debug_Line;

      File : Ada.Text_IO.File_Type;

      use Units.Dependency_Lists;
      use Ada.Strings.Unbounded;
      use Ada_Lexer;

      Tmp2                 : Units.Dependency_Lists.List := Units.Dependency_Lists.Empty_List;
      Unit_Kind2           : Units.Unit_Kind;
      -- Fixme: to be renamed

      Unit_Type_Identified : Boolean               := False;
      Implementation       : Boolean               := True;
      Parent_Pkg_Name      : Unbounded_String      := Null_Unbounded_String;
      -- Parent_Pkg_Name should remain null unless subunits (that is "separate")

      -- -----------------------------------------------------------------------
      -- Procedure: Get_Unit_Name
      -- -----------------------------------------------------------------------
      function Get_Unit_Name return String is
         Name : Unbounded_String := Null_Unbounded_String;
      begin
         Name := To_Unbounded_String (Lexeme);
         loop
            Find_Next;
            if Token_ID = Dot_T then
               Find_Next;
               Name := Name & "." & Lexeme;
            else
               exit;
            end if;
         end loop;
         Put_Debug_Line ("Unit = " & To_String (Name));
         return To_String (Name);
      end Get_Unit_Name;

      -- -----------------------------------------------------------------------
      procedure Process_Subunit is
      begin
         Find_Next; -- jump over "("
         Find_Next;
         declare
            Unit : constant String := Get_Unit_Name;
         begin
            Parent_Pkg_Name := To_Unbounded_String (Unit) & '.';
            Put_Debug_Line ("   - separate from " & Unit); -- , Level => Verbose);
         end;
      end Process_Subunit;

      -- -----------------------------------------------------------------------
      procedure Process_With is
      begin
         Unit_List : loop
            Find_Next;
            declare
               Unit : constant String := Get_Unit_Name;
               -- Fixme: to be replaced with the US version of Get_Unit_Name
            begin
               Put_Debug_Line ("   - withing " & Unit); -- , Level => Verbose);

               Tmp2.Append ((To_Unit => To_Unbounded_String (Unit),
                             File    => To_Unbounded_String (From_Source),
                             Line    => Ada_Lexer.Line));

               exit Unit_List when Token_ID /= Comma_T;
               -- otherwise, loop to continue in the list of comma separated withed unit
            end;
         end loop Unit_List;
      end Process_With;

      -- -----------------------------------------------------------------------
      procedure Process_Pkg is
      begin
         Unit_Type_Identified := True;

         Put_Debug_Line ("5 : found unit " &  Ada_Token'Image (Token_ID));
         if Is_Empty (Tmp2) then
            -- we reach the unit name without meeting any "with"
            Put_Debug_Line ("No with");
         end if;

         -- processing the package declaration
         Find_Next;
         Put_Debug_Line ("6 : " &  Ada_Token'Image (Token_ID));

         if Token_ID = Body_T then
            -- let's jump over "body" reserved word
            Find_Next;
            Implementation := True;
         else
            Implementation := False;
         end if;

         declare
            Unit : constant String := To_String (Parent_Pkg_Name) & Get_Unit_Name;
         begin
            Put_Debug_Line ("in package " & Unit & " implem : " & Boolean'Image (Implementation));

            Units.Add_Unit (Unit         => (Name           => To_Unbounded_String (Unit),
                                             Lang           => Sources.Ada_2012,
                                             Kind           => Units.Package_K,
                                             Implementation => Implementation),
                            Dependencies => Tmp2);

            -- Let's reset the tmp list. This should be usefull only when processing
            -- a source embedding multiple package declaration, so that the "with" of
            -- the first pkg will not be attributed to following pkg.
            Clear (Tmp2);

         end;
      end Process_Pkg;

      -- -----------------------------------------------------------------------
      procedure Process_Subroutine is
      begin
         Unit_Type_Identified := True;
         if    Token_ID = Procedure_T then Unit_Kind2 := Units.Procedure_K;
         elsif Token_ID = Function_T  then Unit_Kind2 := Units.Function_K;
         end if;

         Put_Debug_Line ("5 : found unit type : " &  Ada_Token'Image (Token_ID));
         if Is_Empty (Tmp2) then
            -- we reach the unit name without meeting any "with"
            Put_Debug_Line ("No with ");
         end if;

         -- processing the subprogram declaration
         Find_Next;
         declare
            Unit : constant String := To_String (Parent_Pkg_Name) & Get_Unit_Name;
            use type Units.Unit_Kind;
         begin
            Put_Debug_Line ("Unit name : " & " " & Unit);
            Put_Debug_Line ("7 : after " & Unit & " : " &  Ada_Token'Image (Token_ID));

            case Token_ID is
               when Renames_T   => Implementation := False;
                  -- "procedure X renames ..."
                  -- package renaming will not be considered implementation (that is, body)

               when Is_T        => Implementation := True;
                  -- "procedure X is ..."

               when Semicolon_T => Implementation := False;
                  -- "procedure X;"

               when others      => Implementation := True;
                  -- Limitation:
                  --** this simple implementation doesn't work if subprogram have parameters,
                  --   it works only when "is" is immedliatly following the subprogram name.
                  --
                  --   Spec vs implementation works this way only for packages
                  --   "package body X is ..." vs "package X is ..."
                  --   But analyzis is more complex for procedures and functions :
                  --   "procedure X (...) is ..." / "procedure X (...);" / "procedure X (...) renames ...;"
                  --   Due to the complex lexical procedure parameters analisys,
                  --   implementation is delayed until...

            end case;

            Put_Debug_Line ("Implem : " & Boolean'Image (Implementation));

            if Unit_Kind2 = Units.Procedure_K then
               -- Fixme : utiliser la syntaxe 2012 pour faire une unique déclaration dans le declare
               Units.Add_Unit
                 (Unit => (Name           => To_Unbounded_String (Unit),
                           Lang           => Sources.Ada_2012,
                           Kind           => Units.Procedure_K,
                           Implementation => Implementation),
                  Dependencies => Tmp2);
            elsif  Unit_Kind2 = Units.Function_K   then
               Units.Add_Unit
                 (Unit => (Name           => To_Unbounded_String (Unit),
                           Lang           => Sources.Ada_2012,
                           Kind           => Units.Function_K,
                           Implementation => Implementation),
                  Dependencies => Tmp2);
            end if;

            -- Let's reset the tmp list. This should be usefull only when processing
            -- a source embedding multiple package declaration, so that the "with" of
            -- the first pkg will not be attributed to following pkg.
            Clear (Tmp2);

         end;
      end Process_Subroutine;

      -- -----------------------------------------------------------------------
      procedure Process_Task is
      begin
         Unit_Type_Identified := True;

         Put_Debug_Line ("Found unit type : " &  Ada_Token'Image (Token_ID));
         if Is_Empty (Tmp2) then
            -- we reach the unit name without meeting any "with"
            Put_Debug_Line ("No with ");
         end if;

         Find_Next;
         if Token_ID = Body_T then
            -- let's jump over "body" reserved word
            Find_Next;
         end if;

         -- processing the task declaration
         declare
            Unit : constant String := To_String (Parent_Pkg_Name) & Get_Unit_Name;
         begin
            Put_Debug_Line ("Task name : " & " " & Unit);

            Units.Add_Unit
              (Unit         => (Name           => To_Unbounded_String (Unit),
                                Lang           => Sources.Ada_2012,
                                Kind           => Units.Task_K,
                                Implementation => True), -- separate task body
               Dependencies => Tmp2);

            -- Let's reset the tmp list. This should be usefull only when processing
            -- a source embedding multiple package declaration, so that the "with" of
            -- the first pkg will not be attributed to following pkg.
            Clear (Tmp2);

         end;
      end Process_Task;

      -- -----------------------------------------------------------------------
      procedure Process_Protected is
      begin
         Unit_Type_Identified := True;

         Put_Debug_Line ("Found unit type : " &  Ada_Token'Image (Token_ID));
         if Is_Empty (Tmp2) then
            -- we reach the unit name without meeting any "with"
            Put_Debug_Line ("No with ");
         end if;

         Find_Next;
         if Token_ID = Body_T then
            -- let's jump over "body" reserved word
            Find_Next;
         end if;

         declare
            Unit : constant String := To_String (Parent_Pkg_Name) & Get_Unit_Name;
         begin
            Put_Debug_Line ("Protected name : " & " " & Unit);

            Units.Add_Unit
              (Unit         => (Name           => To_Unbounded_String (Unit),
                                Lang           => Sources.Ada_2012,
                                Kind           => Units.Protected_K,
                                Implementation => True), -- separate protected body
               Dependencies => Tmp2);

            -- Let's reset the tmp list. This should be usefull only when processing
            -- a source embedding multiple package declaration, so that the "with" of
            -- the first pkg will not be attributed to following pkg.
            Clear (Tmp2);

         end;
      end Process_Protected;

      In_Generic_Formal_Part : Boolean := False;
      -- Set to True after "generic" keyword so that to avoid considering "with" declaration
      -- in formal parameters, like in :
      -- > generic
      -- >   with procedure X;
      -- > package Y ...

   begin
      -- -----------------------------------------------------------------------
      Put_Debug_Line ("Looking for dependencies in " & From_Source & " :"); -- , Level => Verbose);

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => From_Source);
      Set_Input_Feeder (File);

      Source_Analysis : loop
         Put_Debug_Line ("Loop : " &  Ada_Token'Image (Token_ID) & " [" & Natural'Image (Line) & "," & Natural'Image (Column) & "]"); -- & Lexeme);

         begin
            -- The withed units are first stored in
            -- a Tmp dependency list, with the Unit_Name left blank,
            -- When the unit name is met, the Tmp list is modified to
            -- set the Unit_Name, then added to the returned dependency list.
            -- Limitation: only the fist Ada unit per source file is taken into account
            -- Limitation: spec vs body recognition do not work for procedures, fonctions and so on.

            -- Ada units are :
            -- - procedure
            -- - function
            -- - package
            -- Each of them can be spec, body, generic, instantiation or renaming (including generic renaming)
            -- And body subunits, that is separate bodies for :
            -- - procedure
            -- - function
            -- - package
            -- - task
            -- - protected
            -- Exemple :
            -- > separate (Y)
            -- > protected body X is
            --

            case Token_ID is
               when With_T =>
                  if In_Generic_Formal_Part then
                     Put_Debug_Line ("skiping with in generic formal part");
                     Find_Next; -- To skip the "with"
                     Put_Debug_Line ("Skip the with, token = " &  Ada_Token'Image (Token_ID));
                     Find_Next; -- To skip the following "procedure" or whatever
                     Put_Debug_Line ("Skip what follow the with, token = " &  Ada_Token'Image (Token_ID));
                  else
                     Process_With;
                  end if;

               when Separate_T =>
                  Process_Subunit;

               when Package_T =>
                  Process_Pkg;
                  exit Source_Analysis;
                  -- This optimization (exiting before end of file) prevents multiple pkg per file processing.
                  -- On the other hand, GtkAda analysis drop from 8s to 0.7s when uncommenting this line.
                  -- Chechekd with :
                  -- > time ../../Obj/archicheck -lf -I gtkada

               when Procedure_T | Function_T =>
                  Process_Subroutine;
                  exit Source_Analysis; -- See optimization note above.

               when Protected_T =>
                  Process_Protected;
                  exit Source_Analysis; -- See optimization note above.

               when Task_T =>
                  Process_Task;
                  exit Source_Analysis; -- See optimization note above.

               when Generic_T =>
                  Put_Debug_Line ("In_Generic_Formal_Part := True, token = " & Ada_Token'Image (Token_ID));
                  In_Generic_Formal_Part := True;
                  Find_Next; -- To skip the "generic" word

               when others =>
                  Find_Next;

            end case;

         exception
            when Error : others =>
               IO.Put_Error (IO.GNU_Prefix (From_Source, Line, Column) & "parse exception:");
               IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
         end;

         exit Source_Analysis when Token_ID = End_of_File_T;

      end loop Source_Analysis;

      if not Unit_Type_Identified then IO.Put_Warning ("Unknown Ada Unit in " & From_Source); end if;

      Ada.Text_IO.Close (File);

   end Analyze_Dependencies;

end Archicheck.Lang.Ada_Processor;
