-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Archicheck.Analyze_Rules body
-- This first version of the analyser is based on predefined Ada string packages.
-- -----------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Archicheck.Settings;
with Archicheck.Get_Dependencies;
with Archicheck.IO;

procedure Archicheck.Analyze_Rules (From_File  : in  String;
                                    Components : out Component_Maps.Map)
is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False;
                             Prefix : in String  := "Analyses_Rules") renames Archicheck.IO.Put_Debug_Line;
   procedure Put_Debug (Msg    : in String  := "";
                        Debug  : in Boolean := False;
                        Prefix : in String  := "Analyses_Rules") renames Archicheck.IO.Put_Debug;

   The_Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ,;-");

   -- -------------------------------------------------------------------------
   function To_Lower (Source  : String) return String is
   begin
      return Translate (Source,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;

   -- -------------------------------------------------------------------------
   function Is_Unit_In_Component (Unit      : String;
                                  Component : String) return Boolean
   is
      procedure Put_Unit (Position : Unit_Lists.Cursor) is
      begin
         Put_Debug (To_String (Unit_Lists.Element (Position)) & " ");
      end Put_Unit;

      Units : Unit_Lists.List;
      Found : Boolean := False;

   begin
      Put_Debug_Line ("Is " & Unit & " in " & Component & "? ");

      if Component_Maps.Contains (Components, Component) then
         -- the component was described by one or more declarations
         Units := Component_Maps.Element (Components, Component);
         Found := Unit_Lists.Contains (Units, To_Unbounded_String (Unit));

         Put_Debug ("Is " & Unit & " in ");
         Unit_Lists.Iterate (Units, Put_Unit'Access);
         Put_Debug ("?");
         if Found then
            Put_Debug_Line (" YES!");
         else
            Put_Debug_Line (" NO!");
         end if;
         Put_Debug_Line (Boolean'Image (Found));

         return Found;

      elsif To_Lower (Head (Unit, Count => Component'Length)) =
        To_Lower (Component)
      then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("YES");
         return True;

      else
         Put_Debug_Line ("NO");
         return False;

      end if;
   end Is_Unit_In_Component;

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

   Idx : Natural;

   Rules_File : Ada.Text_IO.File_Type;

begin
   Ada.Text_IO.Open (File => Rules_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => From_File);

   while not Ada.Text_IO.End_Of_File (Rules_File) loop
      declare
         Line : constant String
           := Trim (Ada.Text_IO.Get_Line (File => Rules_File), Side => Both);
      begin
         -- let's avoid comment line
         if Head (Line, Count => 2) = "--" then
            null; -- it's a comment line

         -- and empty line
         elsif Index_Non_Blank (Line) = 0 then
            null; -- it's a blank line

         else
            Idx := Index (Source  => Line,
                          Pattern => " contains ",
                          Going   => Forward,
                          Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
            -- it's a "contains" line, so let's get
            if Idx /= 0 then
               --                 Put_Debug_Line ("Idx = " & Natural'Image (Idx));
               --     Put_Debug_Line ("Line'Last = " & Natural'Image (Line'Last));
               --                 Put_Debug_Line ("Line (Idx + 9 .. Line'Last) = >"
               --                           & Line (Idx + 9 .. Line'Last)
               --                           & "<");
               declare
                  Component_Name : constant String := Get_Component_Name (Line);
                  Unit_List      : Unit_Lists.List
                    := Get_Unit_List (Line (Idx + 9 .. Line'Last));
                  C              : Component_Maps.Cursor;

                  procedure Add_Units (Component : in     String;
                                       Units     : in out Unit_Lists.List) is
                     pragma Unreferenced (Component);
                     procedure Copy_Unit (Position : Unit_Lists.Cursor) is
                     begin
                        Unit_Lists.Append (Units,
                                           Unit_Lists.Element (Position));
                     end Copy_Unit;

                  begin
                     Unit_Lists.Iterate (Unit_List, Copy_Unit'Access);
                  end Add_Units;

                  procedure Put_Unit (Position : Unit_Lists.Cursor) is
                  begin
                     Put_Debug (To_String (Unit_Lists.Element (Position)) & " ");
                  end Put_Unit;

               begin
                     Put_Debug_Line ("Line           >" & Line & "<");
                     Put_Debug_Line ("Component_Name >" & Component_Name & "<");
                     Put_Debug      ("Units          >");
                     Unit_Lists.Iterate (Unit_List, Put_Unit'Access);
                     Put_Debug_Line ("<");
                     Put_Debug_Line;

                  if Component_Name = "" then
                     null; -- probaly an empty line

                  else
                     C := Component_Maps.Find (Components, Component_Name);

                     if Component_Maps.Has_Element (C) then
                        Component_Maps.Update_Element (Components, C, Add_Units'Access);

                     else
                        Component_Maps.Insert (Components,
                                               Component_Name,
                                               Unit_List);
                     end if;
                     Unit_Lists.Clear (Unit_List);
                  end if;

               end;
            else --** il faudrait v�rifier, mais on suppose pour l'instant que
                 -- c'est la description d'une couche
               null;
               Idx := Index (Source  => Line,
                             Pattern => " is a layer over ",
                             Going   => Forward,
                             Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
               if Idx /= 0 then
                  declare
                     Client : constant String := Get_Component_Name (Line);
                     Server : constant String
                       := Get_Component_Name (Line (Idx + 16 .. Line'Last));
                  begin
                     -- =======================================================
                     Put_Debug_Line (Client & " utilise " & Server);

                     declare
                        procedure Check_Dependency (Position : Dependency_Lists.Cursor) is
                           X : constant String := To_String (Dependency_Lists.Element
                                                             (Position).Unit_Name);
                           Y : constant String := To_String (Dependency_Lists.Element
                                                             (Position).Depends_On_Unit);
                        begin
                           Put_Debug_Line (X & " depends on " & Y);
                           Put_Debug_Line ("if " & Y & " is in "
                                           & Server & " then " & X
                                           & " is     in " & Client);

                           if Is_Unit_In_Component (Unit      => Y,
                                                    Component => Server) and not
                             Is_Unit_In_Component (Unit => X, Component => Client)
                           then
                              IO.Put_Error
                                (X & " is not in "
                                 & Client & " layer, and so shall not directly use "
                                 & Server & " layer");
                           end if;

                           Put_Debug_Line ("if " & X & " is in "
                                            & Server & " then " & Y
                                            & " is NOT in " & Client);

                           if Is_Unit_In_Component (Unit => X, Component => Server) and
                             Is_Unit_In_Component (Unit => Y, Component => Client)
                           then
                              IO.Put_Error (X & " is in " & Server
                                            & " layer, and so shall not use the upper "
                                            & Client & " layer");
                           end if;

                           Put_Debug_Line ("if " & X & " is in "
                                           & Client & " then " & Y
                                           & " should be in either "
                                           & Client & " or " & Server);

                           if Is_Unit_In_Component (Unit => X, Component => Client) and not
                             (Is_Unit_In_Component (Unit => Y, Component => Client) or
                              Is_Unit_In_Component (Unit => Y, Component => Server))
                           then
                              IO.Put_Warning (X & " (in " & Client & " layer) uses "
                                              & Y &
                                                " that is neither in the same layer, nor in the lower "
                                              & Server & " layer");
                           end if;

                        end Check_Dependency;

                        procedure Analyze_Source (Position : Source_Lists.Cursor) is
                           Source_Name : constant String :=
                             To_String (Source_Lists.Element (Position).Name);
                           Dependencies : Dependency_Lists.List;
                        begin
                           Dependencies := Get_Dependencies (Source_Name);
                           Dependency_Lists.Iterate (Container => Dependencies,
                                                     Process   => Check_Dependency'Access);
                        end Analyze_Source;

                        Sources : Source_Lists.List;

                     begin
                        Sources := Settings.Source_List;
                        Source_Lists.Iterate (Container => Sources,
                                              Process   => Analyze_Source'Access);
                     end;

                     -- =======================================================
                  end;
               else
                  IO.Put_Error ("Quezako : >" & Line & "<"); --**
               end if;


            end if;
         end if; -- /= comment
      end;
   end loop;
   Ada.Text_IO.Close (Rules_File);

end Archicheck.Analyze_Rules;
