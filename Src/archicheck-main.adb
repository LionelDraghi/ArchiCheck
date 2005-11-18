with Ada.Command_Line;
with Ada.Text_IO;
-- with Ada.Directories;
with Archicheck.Cmd_Line;
with Archicheck.Dependency_List;
with Archicheck.Get_Dependencies;
with Archicheck.Source_List;
with Archicheck.Source_List_IO;

procedure Archicheck.Main is
   Cmd_Line_OK : Boolean;
   Sources     : Source_List.List;

begin
   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if Cmd_Line_OK then
--        if not Ada.Directories.Exists (Cmd_Line.Tmp_Dir) then
--           Ada.Directories.Create (Cmd_Line.Tmp_Dir);
--        end if;

      -- 1 - let's get sources
      Sources := Cmd_Line.Source_List;

      if Cmd_Line.List_Files then
         Source_List_IO.Dump_Sources (Sources);
      end if;

      -- 2 - let's extract dependencies from sources
      declare
         procedure Put_Dependency (Position : Dependency_List.Cursor) is
            use Ada.Strings.Unbounded;
            use Ada.Text_IO;
         begin
            if Cmd_Line.List_Dependencies then
               Put_Line
                 (To_String (Dependency_List.Element (Position).Unit_Name) &
                  " depends on " &
                  To_String (Dependency_List.Element (Position).Depends_On_Unit));
            end if;
         end Put_Dependency;

         procedure Analyze_Source (Position : Source_List.Cursor) is
            use Ada.Strings.Unbounded;
            Source_Name : constant String :=
                            To_String (Source_List.Element (Position).Name);
            Dependencies : Dependency_List.List;
         begin
            Dependencies := Get_Dependencies (Source_Name);
            Dependency_List.Iterate (Container => Dependencies,
                                     Process   => Put_Dependency'Access);
         end Analyze_Source;

      begin
         Source_List.Iterate (Container => Sources,
                              Process   => Analyze_Source'Access);
      end;

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Archicheck.Main;
