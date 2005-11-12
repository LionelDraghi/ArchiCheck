with Archicheck.Source_List;

private package Archicheck.Cmd_Line is

   procedure Analyze_Cmd_Line (Line_OK : out Boolean);
   function Source_List return Archicheck.Source_List.List;

end Archicheck.Cmd_Line;
