-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Archicheck.Main body

with Ada.Command_Line;
--with Ada.Text_IO;
with Archicheck.Cmd_Line;
with Archicheck.Settings;
with Archicheck.Get_Dependencies;
with Archicheck.Source_Lists_IO;
with Archicheck.Analyze_Rules; -- first version
-- with Archicheck.Rules_Parser;
with Archicheck.IO;

procedure Archicheck.Main is
   Cmd_Line_OK   : Boolean;
   Sources       : Source_Lists.List;
   Component_Map : Component_Maps.Map;

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   --     procedure Put_Debug_Line (Msg    : in String  := "";
   --                               Debug  : in Boolean := True;
   --                               Prefix : in String  := "Main") renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug (Msg    : in String  := "";
   --                          Debug  : in Boolean := True;
   --                          Prefix : in String  := "Main") renames Archicheck.IO.Put_Debug;

begin
   Cmd_Line.Analyze_Cmd_Line (Cmd_Line_OK);

   if Cmd_Line_OK then

      -- 1 - let's get sources
      Sources := Settings.Source_List;

      if Settings.List_Files then
         Source_Lists_IO.Dump_Sources (Sources);
      end if;

      -- 2 - let's extract dependencies from sources
      declare
         procedure Analyze_Source (Sources : Source) is
            use Ada.Strings.Unbounded;
            Source_Name : constant String := To_String (Sources.Name);
            Dependencies : Dependency_Lists.List;
            use IO;
         begin
            Dependencies := Get_Dependencies (Source_Name);
            for Dependence of Dependencies loop
               if Settings.List_Dependencies then
                  Put (To_String (Dependence.Unit_Name));
                  if Dependence.Specification then
                     Put (" specification");
                  else
                     Put (" body         ");
                  end if;
                  Put_Line (" depends on " & To_String (Dependence.Depends_On_Unit));
               end if;
            end loop;

         end Analyze_Source;

      begin
         for Src of Sources loop
            Analyze_Source (Src);
         end loop;
      end;

      -- 3 - is there some rules file to analyze?
      if Settings.Rules_File_Name /= "" then

--           -- Rules file parsing :
--           Rules_Parser.Parse (Settings.Rules_File_Name);
--           -- Components => Component_Map);

         -- Simply coded initial version integrating parsing + analyze
         Analyze_Rules (From_File  => Settings.Rules_File_Name,
                        Components => Component_Map);

      end if;

      if Settings.List_Components then

         declare
            use IO;
            use Ada.Strings.Unbounded;

            procedure Put_Unit_List (UL : Unit_Lists.List) is
               First_Unit : Boolean := True;
            begin
               for U of UL loop
                  if First_Unit then
                     Put (To_String (U));
                     First_Unit := False;
                  else
                     Put (" and " & To_String (U));
                  end if;
               end loop;
               New_Line;
            end Put_Unit_List;

         begin
            for C in Component_Map.Iterate loop
               Put (Component_Maps.Key (C) & " contains ");
               Put_Unit_List (Component_Map (C));
            end loop;

         end;
      end if;


   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   end if;

end Archicheck.Main;
