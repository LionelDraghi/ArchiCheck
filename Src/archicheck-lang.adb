-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Lang body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Settings;

with Ada.Directories;
with Ada.Strings.Unbounded;

package body Archicheck.Lang is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False; -- Settings.Debug_Mode,
                             Prefix : in String  := "Lang") renames Archicheck.IO.Put_Debug_Line;

   -- -------------------------------------------------------------------------
   Processor_List : array (Sources.Language) of Interface_Access;

   -- -------------------------------------------------------------------------
   -- Procedure: Get_Src_List
   -- Implementation Notes:
   --    Each plugged language processor will provide the regular expression to
   --    recognize his own files, and a file search is run on that pattern for
   --    each plugged language.

   -- -------------------------------------------------------------------------
   procedure Get_Src_List (Root_Dir : in String) is
      use Ada.Directories;
      Search          : Search_Type;
      Directory_Entry : Directory_Entry_Type;

      use Ada.Strings.Unbounded;

   begin
      for L in Sources.Language loop
         Put_Debug_Line (Msg    => "Analysing directory " & Root_Dir & " for language : " & Sources.Language'Image (L));

         Start_Search
           (Search    => Search,
            Directory => Root_Dir,
            Pattern   => File_Extensions (Processor_List (L).all), -- dispatching call
            Filter    => (Directory => False,
                          others    => True));
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Directory_Entry);
            declare
               Name : constant String := Ada.Directories.Full_Name (Directory_Entry);
            begin
               Sources.Add_Source (Src => (Name => To_Unbounded_String (Name),
                                           -- Time_Tag => Modification_Time (Directory_Entry),
                                           Lang => L));
               Put_Debug_Line (Msg    => "Found " & Name);
            end;

         end loop;
         End_Search (Search);
      end loop;
   end Get_Src_List;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   --
   -- Implementation Notes:
   --   - Based on OpenToken Ada_Lexer
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies is

      -- Change default Debug parameter value to enable/disable Debug messages in this package
      -- -----------------------------------------------------------------------
      procedure Put_Debug_Line (Msg    : in String  := "";
                                Debug  : in Boolean := Archicheck.Settings.Debug_Mode;
                                Prefix : in String  := "Dependencies") renames Archicheck.IO.Put_Debug_Line;

      -- Global text file for reading parse data
      use Ada.Strings.Unbounded;

      Src_List : constant Sources.Source_Lists.List := Sources.Get_List;

   begin
      for S of Src_List loop
         Put_Debug_Line (Msg => "Analysing dependencies for src : " & To_String (S.Name));

         Analyze_Dependencies (Processor_List (S.Lang).all, -- dispatching call
                               From_Source => To_String (S.Name));
      end loop;

      -- if not Unit_Type_Identified then IO.Put_Warning ("Unknown Ada Unit in " & To_String (From_Source)); end if;

   end Analyze_Dependencies;

   -- -------------------------------------------------------------------------
   procedure Subscribe (Language_Processor : in Interface_Access;
                        For_Language       : in Sources.Language) is
   begin
      Processor_List (For_Language) := Language_Processor;
   end Subscribe;

end Archicheck.Lang;
