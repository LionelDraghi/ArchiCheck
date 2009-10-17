with Ada.Text_IO;

package body Archicheck.Source_Lists_IO is

   procedure Dump_Sources (Sources : in Source_Lists.List) is

      procedure Put_Source (Position : Source_Lists.Cursor) is
         use Ada.Strings.Unbounded;
      begin
         Ada.Text_IO.Put_Line (To_String (Source_Lists.Element (Position).Name));
      end Put_Source;

   begin
      Source_Lists.Iterate (Container => Sources,
                            Process   => Put_Source'Access);
   end Dump_Sources;

end Archicheck.Source_Lists_IO;
