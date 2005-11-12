with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

package body Archicheck.Cmd_Line is

   package Internal is
      Source_List : Archicheck.Source_List.List;
   end Internal;

   procedure Analyze_Cmd_Line (Line_OK : out Boolean) is
      use Ada.Command_Line;
      I : Positive := 1;
   begin
      Line_OK := True;
      Options_Analysis : loop
         declare
            Arg : constant String := Argument (I);
            use Ada.Directories;

         begin
            if Arg = "-I" then
               I := I + 1; -- let's jump to directory name
               declare
                  Name : constant String := Full_Name (Argument (I));
               begin
                  if Exists (Name) then
                     if Kind (Name) = Directory then
                        Ada.Text_IO.Put_Line ("Analysing directory " & Name);
                     else
                        Ada.Text_IO.Put_Line (Name & " is not a directory");
                        Line_OK := False;
                        exit Options_Analysis;
                     end if;
                  else
                     Ada.Text_IO.Put_Line ("No " & Name & " directory");
                     Line_OK := False;
                     exit Options_Analysis;
                  end if;

               end;

            else
               Ada.Text_IO.Put_Line ("Unknown option " & Arg);
               Line_OK := False;
               exit Options_Analysis;
            end if;
         end;

         exit Options_Analysis when I = Argument_Count;
         I := I + 1;
      end loop Options_Analysis;

--     exception
--        when others => Line_OK := False;

   end Analyze_Cmd_Line;

   function Source_List return Archicheck.Source_List.List is
   begin
      return Internal.Source_List;
   end Source_List;

end Archicheck.Cmd_Line;
