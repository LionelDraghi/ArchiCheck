with Ada.Command_Line;
with Archicheck.Cmd_Line;
with Archicheck.Source_List;

procedure Archicheck.Main is
   The_List    : Archicheck.Source_List.List;
   Cmd_Line_OK : Boolean;

begin
   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if Cmd_Line_OK then
      The_List := Cmd_Line.Source_List;
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Archicheck.Main;
