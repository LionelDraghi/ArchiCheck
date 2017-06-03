-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


-- Package: Archicheck.Cmd_Line body

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Archicheck.Cmd_Line is

   use Ada.Strings.Unbounded;

   -- -------------------------------------------------------------------------
   Arg_Counter : Positive := 1;

   -- -------------------------------------------------------------------------
   package Local is
      Source_List       : Archicheck.Source_Lists.List;
      List_Files        : Boolean := False;
      List_Dependencies : Boolean := False;
      List_Components   : Boolean := False;
      Rules_File_Name   : Unbounded_String;
   end Local;

   -- -------------------------------------------------------------------------
   procedure Process_Directory_Option (Line_OK : out Boolean) is
   begin
      if Ada.Command_Line.Argument_Count < Arg_Counter + 1 then
         Ada.Text_IO.Put_Line ("Argument missing after -I");
         Line_OK := False;
      else
         declare
            use Ada.Command_Line;
            use Ada.Directories;
            Name : constant String := Full_Name (Argument (Arg_Counter + 1));

         begin
            if Exists (Name) then
               if Kind (Name) = Directory then
                  -- Ada.Text_IO.Put_Line ("Analysing directory " & Name);
                  declare
                     Search : Search_Type;
                     Directory_Entry : Directory_Entry_Type;
                  begin
                     Start_Search
                       (Search    => Search,
                        Directory => Name,
                        Pattern   => "*.ad[sb]", --** hard coded for usual Ada convention
                        Filter    => (Directory => False,
                                      others    => True));
                     while More_Entries (Search) loop
                        Get_Next_Entry (Search, Directory_Entry);
                        Archicheck.Source_Lists.Append
                          (Local.Source_List,
                           (Name     =>
                              To_Unbounded_String
                                (Full_Name (Directory_Entry)),
                            Time_Tag =>
                              Modification_Time (Directory_Entry)));
                        -- Ada.Text_IO.Put_Line ("trouvé : " & Full_Name (Directory_Entry));
                     end loop;
                     End_Search (Search);
                     Line_OK := True;
                  end;

               else
                 Ada.Text_IO.Put_Line (Name & " is not a directory");
                 Line_OK := False;
               end if;
            else
               Ada.Text_IO.Put_Line ("No " & Name & " directory");
               Line_OK := False;
            end if;
         end;
         Arg_Counter := Arg_Counter + 2;
      end if;

   end Process_Directory_Option;

   -- -------------------------------------------------------------------------
   procedure Put_Help is
      use Ada.Text_IO;
   begin
      Put_Line ("Normal use :");
      Put_Line ("   archicheck rules_file [-I directory]*");
      Put_Line ("Debug use :");
      Put_Line ("   archicheck rules_file --list_components");
      Put_Line ("   archicheck [--list_files|--list_dependencies] [-I directory]* ");

      -- previous cmd line format :
      --        Put_Line ("archicheck [-I directory]* options rules_file");
      --        Put_Line ("  options :");
      --        Put_Line ("  --list_files        : list analyzed sources");
      --        Put_Line ("  --list_dependencies : list identified dependencies");
      --        Put_Line ("  --list_components   : list components identified in rules file");

   end Put_Help;

   -- -------------------------------------------------------------------------
   procedure Analyze_Cmd_Line (Line_OK : out Boolean) is
   begin
      Line_OK := True;

      if Ada.Command_Line.Argument_Count < 1 then
         Put_Help;
         Line_OK := False;
         return; --** pas glop
      end if;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "-I" then
               Process_Directory_Option (Line_OK);

            elsif Opt = "--list_files" then
               Local.List_Files := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "--list_dependencies" then
               Local.List_Dependencies := True;
               Arg_Counter := Arg_Counter + 1;

            elsif Opt = "--list_components" then
               Local.List_Components := True;
               Arg_Counter := Arg_Counter + 1;

            -- elsif Arg_Counter = Ada.Command_Line.Argument_Count then
            elsif Ada.Directories.Exists (Opt) then
               Local.Rules_File_Name := To_Unbounded_String (Opt);
               Arg_Counter := Arg_Counter + 1;
            else
               Ada.Text_IO.Put_Line ("Unknown rules file or unknow option " & Opt);
               Put_Help;
               Line_OK := False;
            end if;

--              else
--                 Ada.Text_IO.Put_Line ("Unknown option " & Opt);
--                 Put_Help;
--                 Line_OK := False;

--            end if;
            exit when not Line_OK;
         end;
      end loop;

   end Analyze_Cmd_Line;

   -- -------------------------------------------------------------------------
   function Source_List return Source_Lists.List is
   begin
      return Local.Source_List;
   end Source_List;

   -- -------------------------------------------------------------------------
   function Tmp_Dir return String is
   begin
      return "/tmp"; --** OS specific
   end Tmp_Dir;

   -- -------------------------------------------------------------------------
   function Rules_File_Name  return String is
   begin
      --** controler l'existance de ce fichier!!
      return To_String (Local.Rules_File_Name);
   end Rules_File_Name;


   -- -------------------------------------------------------------------------
   function List_Files return Boolean is
   begin
      return Local.List_Files;
   end List_Files;

   -- -------------------------------------------------------------------------
   function List_Dependencies return Boolean is
   begin
      return Local.List_Dependencies;
   end List_Dependencies;

   -- -------------------------------------------------------------------------
   function List_Components return Boolean is
   begin
      return Local.List_Components;
   end List_Components;

end Archicheck.Cmd_Line;
