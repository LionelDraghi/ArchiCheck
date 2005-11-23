with Ada.Text_IO;

package body Archicheck.Source_Lists_IO is

   procedure Dump_Sources (Sources : in Source_Lists.List)
   is
      use Ada.Text_IO;

      procedure Put_Source (Position : Source_Lists.Cursor) is
         use Ada.Strings.Unbounded;
      begin
         Put_Line (To_String (Source_Lists.Element (Position).Name));
      end Put_Source;

   begin
      -- Create (File, Mode => Out_File, Name => File_Name);
      Source_Lists.Iterate (Container => Sources,
                           Process   => Put_Source'Access);
      -- Close (File);
   end Dump_Sources;

end Archicheck.Source_Lists_IO;
