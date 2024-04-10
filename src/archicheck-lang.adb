-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
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
with Ada.Containers;

package body Archicheck.Lang is

   -- Change default Debug parameter value to enable/disable
   -- Debug messages in this package
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line
     (Msg    : in String  := "";
      Debug  : in Boolean := Settings.Debug_Mode;
      Prefix : in String  := "Lang") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   Processor_List : array (Sources.Language) of Interface_Access;

   -- --------------------------------------------------------------------------
   -- Procedure: Get_Src_List
   -- Implementation Notes:
   --    Each plugged language processor will provide the regular expression to
   --    recognize his own files, and a file search is run on that pattern for
   --    each plugged language.
   -- --------------------------------------------------------------------------
   procedure Get_Src_List (Root_Dir  : in String;
                           Recursive : in Boolean) is

      -- -----------------------------------------------------------------------
      Src_Count : array (Sources.Language) of Natural := [others => 0];
      Dir_Count : array (Sources.Language) of Natural := [others => 1];

      use Ada.Directories;

      Current : constant String := Current_Directory;

      -- -----------------------------------------------------------------------
      procedure Walk (Name : String; L : Sources.Language) is
         -- code mostly from :
         -- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

         Extension : constant String
           := File_Extensions (Processor_List (L).all); -- dispatching call

         -- --------------------------------------------------------------------
         procedure Print (Item : Directory_Entry_Type) is
            -- Fixme: rename Print
            Name : constant String := Full_Name (Item);
            use type Sources.File_Name;
         begin
            if Name'Length > Current'Length and then
              Name (Name'First .. Name'First + Current'Length - 1) = Current
            -- Simple optimization : if the long path is a subdir of the
            -- current one, we only print the subdir
            then
               Sources.Add_Source
                 (Src =>
                    (File => +(Name
                     (Name'First + Current'Length + 1 .. Name'Last)),
                       -- Time_Tag => Modification_Time (Directory_Entry),
                     Lang => L));
            else
               Sources.Add_Source
                 (Src =>
                    (File => +(Name),
                       -- Time_Tag => Modification_Time (Directory_Entry),
                     Lang => L));
            end if;
            Src_Count (L) := Src_Count (L) + 1;
         end Print;

         -- --------------------------------------------------------------------
         procedure Walk (Item : Directory_Entry_Type) is
         begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               -- This is OK with Unix and Windows dir, so I consider
               -- it as portable.
               Dir_Count (L) := Dir_Count (L) + 1;
               Walk (Full_Name (Item), L);
            end if;
         exception when Name_Error => null;
         end Walk;

      begin
         Search (Name, Extension, [Directory => False, others => True], Print'Access);
         if Recursive then
            Search (Name, "", [Directory => True, others => False], Walk'Access);
         end if;
      end Walk;

   begin
      for L in Sources.Language loop
         Put_Debug_Line (Msg => "Analyzing directory " & Root_Dir
                          & " for language : " & Sources.Language'Image (L));
         Walk (Root_Dir, L);
         if Src_Count (L) /= 0 then
            IO.Put_Line (Item => "Found " & Integer'Image (Src_Count (L)) &
                           " "  & Sources.Language'Image (L) & " src in" &
                           Natural'Image (Dir_Count (L)) & " dir",
                         Level => IO.Verbose);
         end if;
      end loop;

   end Get_Src_List;

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Dependencies
   -- --------------------------------------------------------------------------
   procedure Analyze_Dependencies is
      Src_List : constant Sources.Source_Lists.List := Sources.Get_List;
      use type Sources.File_Name;

   begin
      Put_Debug_Line ("Analyzing dependencies," &
                        Ada.Containers.Count_Type'Image
                        (Sources.Source_Lists.Length (Src_List)) & " sources");
      for S of Src_List loop
         Put_Debug_Line ("Analyzing dependencies in " & (+S.File));
         Analyze_Dependencies (Processor_List (S.Lang).all, -- dispatching call
                               From_Source => S.File);
      end loop;

   end Analyze_Dependencies;

   -- --------------------------------------------------------------------------
   -- procedure Register
   -- --------------------------------------------------------------------------
   procedure Register (Language_Processor : in Interface_Access;
                       For_Language       : in Sources.Language) is
   begin
      Processor_List (For_Language) := Language_Processor;
   end Register;

end Archicheck.Lang;
