with Ada.Command_Line;             use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;

procedure main is
file : ada.text_io.File_Type;
file_name : constant String := Argument (1);
begin
open (file, file_name);
set_input (file);
  Rules_File : Ada.Text_IO.File_Type;

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   function Is_Empty
     (Line : in String) return Boolean is separate;
   function Is_A_Comment
     (Line : in String) return Boolean is separate;
   function Is_A_Component_Declaration
     (Line : in String) return Boolean is separate;
   function Is_A_Layer_Rule
     (Line : in String) return Boolean is separate;
   function Is_A_Dependency_Rule
     (Line : in String) return Boolean is separate;
   function Is_A_Use_Restriction_Rule
     (Line : in String) return Boolean is separate;
   procedure Analyze_Component_Declaration
     (Line : in String) is separate;

   The_Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ,;-");

   -- -------------------------------------------------------------------------
   function To_Lower (Source  : String) return String is
   begin
      return Translate (Source,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;

   -- -------------------------------------------------------------------------
   -- Function: Get_Component_Name
   function Get_Component_Name (From_Line : in String) return String is
      First : Positive;      -- Index of first character in token --***
      Last  : Natural := 0;  -- Index of last character in token (also used
                             -- incremented by 1 as the starting point for next
                             -- search).
   begin
      Find_Token (Source => From_Line,
                  Set    => The_Delimiters,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);
      return (From_Line (First .. Last));
   end Get_Component_Name;

   -- -------------------------------------------------------------------------
   -- Function: Get_Unit_List
   function Get_Unit_List (From_Line : in String) return Unit_Lists.List is
      Units : Unit_Lists.List;
      First : Positive;     -- Index of first character in token --**
      Last  : Natural := From_Line'First;  -- Index of last character in token
                                           -- (also used
                                           -- incremented by 1 as the starting
                                           -- point for next search).

   begin
      -- Limitation: unit name is case-sensitve
      loop
         -- Put_Debug_Line ("Last =" & Natural'Image (Last));
         -- Put_Debug_Line ("From_Line'Last =" & Natural'Image (From_Line'Last));
         -- Put_Debug_Line ("Line = >" & From_Line & "<");
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

      return Units;
   end Get_Unit_List;

begin
   Open (Rules_File, Mode => In_File, Name => File_Name);

   Analysis : while not End_Of_File (Rules_File) loop
      declare
         Line : constant String := Trim (Get_Line (Rules_File), Side => Both);
      begin
         
         -- Put_Line ("Line : >" & Line & "<"); --**
         if Is_A_Comment (Line) then
            null;

         elsif Is_Empty (Line) then
            null;

         elsif Is_A_Component_Declaration (Line) then
            Analyze_Component_Declaration (Line);

         elsif Is_A_Layer_Rule (Line) then
            null;

         elsif Is_A_Dependency_Rule (Line) then
            null;

         elsif Is_A_Use_Restriction_Rule (Line) then
            null;

         else
            -- OK := False;
            Put_Line ("*** Quezako : >" & Line & "<"); --**
            exit Analysis;

         end if;

      end;

   end loop Analysis;

   Close (Rules_File);
end main;









separate (Main)

function Is_A_Comment (Line : in String) return Boolean is
begin
   return Head (Line, Count => 2) = "--";
end Is_A_Comment;


separate (Main)

function Is_A_Component_Declaration (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " contains ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Component_Declaration;

separate (Main)

function Is_A_Dependency_Rule (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " depends ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Dependency_Rule;

separate (Main)

function Is_A_Layer_Rule (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " layer ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Layer_Rule;


separate (Main)

function Is_A_Use_Restriction_Rule (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " may_use ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Use_Restriction_Rule;


separate (Main)

function Is_Empty (Line : in String) return Boolean is
begin
   return Index_Non_Blank (Line) = 0;
end Is_Empty;
