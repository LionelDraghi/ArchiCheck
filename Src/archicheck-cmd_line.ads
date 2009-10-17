-- Package: Archicheck.Cmd_Line specification

private package Archicheck.Cmd_Line is

   procedure Analyze_Cmd_Line (Line_OK : out Boolean);

   function Source_List return Source_Lists.List;
   function Tmp_Dir     return String;
   function Rules_File_Name return String;
   function List_Files        return Boolean;
   function List_Dependencies return Boolean;
   function List_Components   return Boolean;

end Archicheck.Cmd_Line;
