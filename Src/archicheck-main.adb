with Ada.Command_Line;
with Ada.Text_IO;
-- with Ada.Directories;
with Archicheck.Analyze_Rules;
with Archicheck.Cmd_Line;
with Archicheck.Dependency_Lists;
with Archicheck.Get_Dependencies;
with Archicheck.Source_Lists;
with Archicheck.Source_Lists_IO;

procedure Archicheck.Main is
   Cmd_Line_OK   : Boolean;
   Sources       : Source_Lists.List;
   Component_Map : Component_Maps.Map;

begin
   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if Cmd_Line_OK then
--        if not Ada.Directories.Exists (Cmd_Line.Tmp_Dir) then
--           Ada.Directories.Create (Cmd_Line.Tmp_Dir);
--        end if;

      -- 1 - let's get sources
      Sources := Cmd_Line.Source_List;

      if Cmd_Line.List_Files then
         Source_Lists_IO.Dump_Sources (Sources);
      end if;

      -- 2 - let's extract dependencies from sources
      declare
         procedure Put_Dependency (Position : Dependency_Lists.Cursor) is
            use Ada.Strings.Unbounded;
            use Ada.Text_IO;
            D : constant Dependency := Dependency_Lists.Element
              (Position);
         begin
            if Cmd_Line.List_Dependencies then
               Put (To_String (D.Unit_Name));
               if D.Specification then
                  Put (" specification");
               else
                  Put (" body         ");
               end if;
               Put_Line (" depends on " &
                         To_String (D.Depends_On_Unit));
            end if;
         end Put_Dependency;

         procedure Analyze_Source (Position : Source_Lists.Cursor) is
            use Ada.Strings.Unbounded;
            Source_Name : constant String :=
                            To_String (Source_Lists.Element (Position).Name);
            Dependencies : Dependency_Lists.List;
         begin
            Dependencies := Get_Dependencies (Source_Name);
            Dependency_Lists.Iterate (Container => Dependencies,
                                      Process   => Put_Dependency'Access);
         end Analyze_Source;

      begin
         Source_Lists.Iterate (Container => Sources,
                               Process   => Analyze_Source'Access);
      end;

      -- 3 - is there some rules file to analyze?
      if Cmd_Line.Rules_File_Name /= "" then
         Analyze_Rules (From_File  => Cmd_Line.Rules_File_Name,
                        Components => Component_Map);
      end if;

      if Cmd_Line.List_Components then
         declare

            procedure Put_Component (Position : Component_Maps.Cursor) is
               First_Unit : Boolean := True;
               use Ada.Text_IO;
               procedure Put_Unit (Position : Unit_Lists.Cursor) is
                  use Ada.Strings.Unbounded;
               begin
                  if First_Unit then
                     Put (To_String (Unit_Lists.Element (Position)));
                     First_Unit := False;
                  else
                     Put (" and " & To_String (Unit_Lists.Element (Position)));
                  end if;

               end Put_Unit;
            begin
               Put (Component_Maps.Key (Position) & " contains ");
               Unit_Lists.Iterate
                 (Container => Component_Maps.Element (Position),
                  Process   => Put_Unit'Access);
               New_Line;
            end Put_Component;
         begin
            Component_Maps.Iterate (Container => Component_Map,
                                    Process   => Put_Component'Access);

         end;
      end if;

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Archicheck.Main;
