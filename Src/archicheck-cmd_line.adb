with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Archicheck.Source_List;

package body Archicheck.Cmd_Line is

   -- -------------------------------------------------------------------------
   Arg_Counter : Positive := 1;

   -- -------------------------------------------------------------------------
   package Local is
      Source_List : Archicheck.Source_List.List;
      List_Files     : Boolean := False;
      List_Dependencies : Boolean := False;
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
                     use Ada.Directories;
                     use Ada.Strings.Unbounded;
                     Search : Search_Type;
                     Directory_Entry : Directory_Entry_Type;
                  begin
                     Start_Search
                       (Search    => Search,
                        Directory => Name,
                        Pattern   => "*.ad[sb]",
                        Filter    => (Directory => False,
                                      others    => True));
                     while More_Entries (Search) loop
                        Get_Next_Entry (Search, Directory_Entry);
                        Archicheck.Source_List.Append
                          (Local.Source_List,
                           (Name     =>
                              To_Unbounded_String
                                (Full_Name (Directory_Entry)),
                            Time_Tag =>
                              Modification_Time (Directory_Entry)));
                     end loop;
                     End_Search (Search);
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
      Put_Line ("archicheck [-I directory] options");
      Put_Line ("  options :");
      Put_Line ("  --list_files        : list analyzed sources");
      Put_Line ("  --list_dependencies : list identified dependencies");
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

            else
               Ada.Text_IO.Put_Line ("Unknown option " & Opt);
               Put_Help;
               Line_OK := False;

            end if;
            exit when not Line_OK;
         end;
      end loop;

   end Analyze_Cmd_Line;

   -- -------------------------------------------------------------------------
   function Source_List return Archicheck.Source_List.List is
   begin
      return Local.Source_List;
   end Source_List;

   -- -------------------------------------------------------------------------
   function Tmp_Dir return String is
   begin
      return "/tmp";
   end Tmp_Dir;

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

end Archicheck.Cmd_Line;
