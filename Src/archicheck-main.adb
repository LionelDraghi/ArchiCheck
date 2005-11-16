with Ada.Command_Line;
-- with Ada.Directories;
with Archicheck.Cmd_Line;
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

      if Cmd_Line.Dump_Source_List then
         Source_List_IO.Dump_Sources
           (File_Name => Cmd_Line.Source_List_File_Name,
            Sources   => Sources);
      end if;

      -- 2 - let's extract dependencies from sources

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Archicheck.Main;
