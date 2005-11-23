with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

procedure Archicheck.Analyze_Rules (From_File  : in  String;
                                    Components : out Component_Maps.Map)
is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   Rules_File : File_Type;

   Debug : constant Boolean := False;

   -- -------------------------------------------------------------------------
   function Get_Component_Name (From_Line : in String) return String is
      Idx : Natural;
   begin
      Idx := Index (Source  => From_Line,
                    Pattern => "contains",
                    Going   => Forward,
                    Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      return Trim (From_Line (1 .. Idx - 1),
                   Side => Both);
   end Get_Component_Name;

   -- -------------------------------------------------------------------------
   function Get_Unit_List (From_Line : in String) return Unit_Lists.List is
      Idx   : Natural;
      Units : Unit_Lists.List;
      First : Positive;     -- Index of first character in token
      Last : Natural := 0;  -- Index of last character in token (also used
      -- incremented by 1 -- as the starting point for next search).
      The_Delimiters : constant Ada.Strings.Maps.Character_Set :=
                         Ada.Strings.Maps.To_Set (" ,;");
   begin
      Idx := Index (Source  => From_Line,
                    Pattern => "contains",
                    Going   => Forward,
                    Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      Last := Idx + 7;
      loop
         Find_Token (Source => From_Line (Last + 1 .. From_Line'Last),
                     Set    => The_Delimiters,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         exit when Last = 0;
         if From_Line (First .. Last) /= "and" and --** Case sensitive
           From_Line (First .. Last) /= "also"
         then
            Unit_Lists.Append
              (Units,
               To_Unbounded_String (From_Line (First .. Last)));
         end if;
      end loop;

--        Idx := Index_Non_Blank (Source => From_Line,
--                                From   => Idx + 8, --** pas élégant!!
--                                Going  => Forward);
--        Idx := Index (Source  => From_Line,
--                      Pattern => "also",
--                      Going   => Forward,
--                      Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
--        Unit_Lists.Append (Units, To_Unbounded_String
--                             (Trim (From_Line (Idx .. From_Line'Last),
--                                    Side => Both)));
    return Units;
   end Get_Unit_List;

begin
   Open (File => Rules_File,
         Mode => In_File,
         Name => From_File);
   while not End_Of_File (Rules_File) loop
      declare
         Line : constant String
           := Get_Line (File => Rules_File);
         Component_Name : constant String := Get_Component_Name (Line);
         Unit_List      : Unit_Lists.List := Get_Unit_List (Line);
         C              : Component_Maps.Cursor;
         procedure Add_Units (Component : in     String;
                              Units     : in out Unit_Lists.List) is
            pragma Unreferenced (Component);
            procedure Copy_Unit (Position : Unit_Lists.Cursor) is
            begin
               Unit_Lists.Append (Units, Unit_Lists.Element (Position));
            end Copy_Unit;

         begin
            Unit_Lists.Iterate (Unit_List, Copy_Unit'Access);
         end Add_Units;

            procedure Put_Unit (Position : Unit_Lists.Cursor) is
            begin
               Put (To_String (Unit_Lists.Element (Position)) & " ");
            end Put_Unit;

      begin
         if Debug then
            Put_Line ("Line           >" & Line & "<");
            Put_Line ("Component_Name >" & Component_Name & "<");
            Unit_Lists.Iterate (Unit_List, Put_Unit'Access);
            New_Line;
            New_Line;
         end if;

         C := Component_Maps.Find (Components, Component_Name);

         if Component_Maps.Has_Element (C) then
           Component_Maps.Update_Element (C, Add_Units'Access);

         else
            Component_Maps.Insert (Components,
                                   Component_Name,
                                   Unit_List);
         end if;
         Unit_Lists.Clear (Unit_List);

      end;
   end loop;
   Close (Rules_File);

end Archicheck.Analyze_Rules;
