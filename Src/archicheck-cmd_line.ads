with Archicheck.Source_List;

private package Archicheck.Cmd_Line is

   procedure Analyze_Cmd_Line (Line_OK : out Boolean);

   function Source_List return Archicheck.Source_List.List;
   function Tmp_Dir     return String;
   function Dump_Source_List return Boolean;
   function Source_List_File_Name return String;

end Archicheck.Cmd_Line;
