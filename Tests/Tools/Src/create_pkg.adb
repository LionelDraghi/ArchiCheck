with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Create_Pkg is

   -- the Create_Pkg utility is used to create dummy Ada packages for ArchiCheck tests

   -- -------------------------------------------------------------------------
   procedure Put_Usage is
   begin
      Put_Line ("usage : Create_Pkg pkg_name spec|body [-in Output_Dir] [-with withed_pkg]");
      Put_Line ("   by default, create only a specification, in the current directory");
      Put_Line ("   -with shall be at the end of the line");
   end Put_Usage;

   use Ada.Strings.Unbounded;

   -- -------------------------------------------------------------------------
   function To_File_Name (Unit_Name : Unbounded_String) return Unbounded_String is
      Tmp : Unbounded_String := Unit_Name;
      Dot_To_Dash_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping (From => ".", To => "-");

   begin
      Translate (Source  => Tmp,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      Translate (Source  => Tmp,
                 Mapping => Dot_To_Dash_Map);
      return Tmp;
   end To_File_Name;

   File_Created : Boolean := False;

   -- -------------------------------------------------------------------------
   Arg_Counter : Positive := 1;
   File : File_Type;
   Create_Body  : Boolean := False;

   Unit_Name : Unbounded_String;
   File_Name : Unbounded_String;
   Dir_Name  : Unbounded_String;

   Version : constant String := "v1.0";

   -- -------------------------------------------------------------------------
   procedure Create_File is
      Tmp : constant String := To_String (Dir_Name & To_File_Name (Unit_Name));
   begin
      if not File_Created then
         if Create_Body then
            Create (File,
                    Mode => Out_File,
                    Name => Tmp & ".adb");
         else
            Create (File,
                    Mode => Out_File,
                    Name => Tmp & ".ads");
         end if;
         File_Created := True;
      end if;
   end Create_File;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   else
      Unit_Name := To_Unbounded_String (Ada.Command_Line.Argument (Arg_Counter));
      Arg_Counter := Arg_Counter + 1;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "spec" then
               Create_Body := False;

            elsif Opt = "body" then
               Create_Body := True;

            elsif Opt = "-in" then
               Arg_Counter := Arg_Counter + 1;
               declare
                  Dir : constant String
                    := Ada.Command_Line.Argument (Arg_Counter);
               begin
                  Dir_Name := To_Unbounded_String (Dir & "/");
                  if not Ada.Directories.Exists (Dir) then
                     Ada.Directories.Create_Directory (Dir);
                  end if;
               end;

            elsif Opt = "-with" then
               Create_File;
               Arg_Counter := Arg_Counter + 1;
               Put_Line (File, "with " &
                         Ada.Command_Line.Argument (Arg_Counter) & ";");

            else
              Put_Line ("Unknown Option " & Opt);
              Put_Usage;
              Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

            end if;

         end;
         Arg_Counter := Arg_Counter + 1;
      end loop;


      Create_File;
      New_Line (File);
      Put_Line (File, "-- created by Create_Pkg " & Version);
      if Create_Body then
         Put_Line (File, "package body " & To_String (Unit_Name) & ";");
      else
         Put_Line (File, "package "      & To_String (Unit_Name) & ";");
      end if;
      Put_Line (File, "   null;");
      Put_Line (File, "end " & To_String (Unit_Name) & ";");
      Close (File);

   end if;

end Create_Pkg;


