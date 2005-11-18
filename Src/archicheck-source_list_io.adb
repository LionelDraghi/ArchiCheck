with Ada.Text_IO;

package body Archicheck.Source_List_IO is

   procedure Dump_Sources (Sources : in Source_List.List)
   is
      use Ada.Text_IO;

      procedure Put_Source (Position : Source_List.Cursor) is
         use Ada.Strings.Unbounded;
      begin
         Put_Line (To_String (Source_List.Element (Position).Name));
      end Put_Source;

   begin
      -- Create (File, Mode => Out_File, Name => File_Name);
      Source_List.Iterate (Container => Sources,
                           Process   => Put_Source'Access);
      -- Close (File);
   end Dump_Sources;

end Archicheck.Source_List_IO;
