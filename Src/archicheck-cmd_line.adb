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
                  Ada.Text_IO.Put_Line ("Analysing directory " & Name);
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
   procedure Analyze_Cmd_Line (Line_OK : out Boolean) is
   begin
      Line_OK := True;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "-I" then
               Process_Directory_Option (Line_OK);

            else
               Ada.Text_IO.Put_Line ("Unknown option " & Opt);
               Line_OK := False;
               exit;

            end if;
         end;
         --** mettre l'usage quand Line_OK = False
         exit when Line_OK = False;
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
   function Dump_Source_List return Boolean is
   begin
      return True;
   end Dump_Source_List;

   -- -------------------------------------------------------------------------
   function Source_List_File_Name return String is
   begin
      return "file_list";
   end Source_List_File_Name;

end Archicheck.Cmd_Line;
