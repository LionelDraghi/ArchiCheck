-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Units body
--
-- Implementation Notes:
--
-- Portability Issues:
--   None
--
-- Anticipated Changes:
--   - More more thorough Ada implementation needed
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Settings;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

package body Archicheck.Units is

   use Ada.Strings.Unbounded;

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line
     (Msg    : in String  := "";
      Debug  : in Boolean := Settings.Debug_Mode; --True; -- change to True to debug this package
      Prefix : in String  := "Units") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   -- Dependency_List : Dependency_Lists.List := Dependency_Lists.Empty_List;

   -- -----------------------------------------------------------------------
   -- Function: To_Lower
   -- -----------------------------------------------------------------------
   function To_Lower (Source  : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Translate (Source,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;

   -- --------------------------------------------------------------------------
   function Location_Image (Dep : Dependency) return String is
     (IO.GNU_Prefix (File => To_String (Dep.File),
                     Line => Dep.Line));

   -- --------------------------------------------------------------------------
   -- Function: Unit_List_Image
   -- --------------------------------------------------------------------------
   function Unit_List_Image (List : Dependency_Lists.List) return String is
      Tmp : Unbounded_String := Null_Unbounded_String;
      use Dependency_Lists;
   begin
      -- the output is of this kind : "X, Y, Z, and V"
      for C in List.Iterate loop
         if C = List.First then
            Tmp := Element (C).To_Unit;
         elsif C = List.Last then
            Tmp := Tmp & " and " & Element (C).To_Unit;
         else
            Tmp := Tmp & ", " & Element (C).To_Unit;
         end if;
      end loop;
      return (To_String (Tmp));
      -- Fixme: same code in Rules, deserve a generic
   end Unit_List_Image;

   -- --------------------------------------------------------------------------
   package Component_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Dependency_Lists.List,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Dependency_Lists."=");
   Component_Map : Component_Maps.Map;
   -- The component map is a fast way to find what contains a Component.
   -- Note that it is maintained in parallel with the Relationship_List.

   -- --------------------------------------------------------------------------
   package Owner_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Dependency,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   Owner_Map : Owner_Maps.Map;
   -- Owner map run the other way round : the key is a Unit that is contained
   -- in a component, and the Element_Type is this component.
   -- Note that this is not a list, as a Unit is not supposed to be in more
   -- Component.
   -- Note that it is also maintained in parallel with the Relationship_List.


   -- --------------------------------------------------------------------------
   -- Function: Is_A_Child
   -- --------------------------------------------------------------------------
   function Is_A_Child (Unit      : String;
                        Component : String) return Boolean
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

   begin
      if To_Lower (Unit) = To_Lower (Component) then
         -- The Unit is the component
         Put_Debug_Line ("Unit " & Unit & " is (in) the component " & Component);
         return True;

      elsif Unit'Length > Component'Length and then
        (To_Lower (Head (Unit, Count => Component'Length)) = To_Lower (Component) and
             Unit (Component'Length + 1) = '.')
      then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("Unit " & Unit & " is (as child) in the component " & Component);
         return True;

      else
         Put_Debug_Line ("Unit " & Unit & " not in component " & Component, Prefix => "");
         return False;

      end if;
   end Is_A_Child;

   -- --------------------------------------------------------------------------
   -- Procedure: Append
   -- --------------------------------------------------------------------------
   --     procedure Append (Dep : Dependency) is
   --        use Dependency_Lists;
   --     begin
   --        Append (Container => Dependency_List,
   --                New_Item  => Dep);
   --     end Append;

   -- --------------------------------------------------------------------------
   -- Function Unit_Description
   -- --------------------------------------------------------------------------
   function Unit_Description (U : in Unit_Attributes) return String is
      use Archicheck.Sources;
   begin
      return (case U.Lang is
                 when Java     => Image (U.Kind),
                 when Ada_2012 => (if U.Implementation
                                   then Image (U.Kind) & " body"
                                   else Image (U.Kind) & " spec"));
   end Unit_Description;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump
   -- --------------------------------------------------------------------------
   procedure Dump is
      use Archicheck.IO;
   begin
      for R of Relationship_List loop
         for D of R.Dependencies loop
            Put_Line (Ada.Strings.Unbounded.To_String (R.From_Unit.Name)
                      & " " & Unit_Description (R.From_Unit)
                      & " depends on "
                      & Ada.Strings.Unbounded.To_String (D.To_Unit),
                      Level => Quiet); --  & Image (D.To.Kind));

            -- gives file details only when verbose :
            Put_Line ("   Found line" & Natural'Image (D.Line)
                      & " in " & Ada.Strings.Unbounded.To_String (D.File),
                      Level => Verbose);
         end loop;
      end loop;

   end Dump;

   -- --------------------------------------------------------------------------
   procedure Add_Unit (Unit         : Unit_Attributes;
                       Dependencies : Dependency_Lists.List) is
   begin
      pragma Assert (Unit not in Component_Attributes,
                     "Call Add_Component instead");
      Put_Debug_Line ("Adding dependencies " & Unit_List_Image (Dependencies)
                      & " to unit " & To_String (Unit.Name));

      Relationship_List.Append ((Unit, Dependencies));
      -- We do not verify that some of those dependencies are not already
      -- registered, as :
      -- 1. this is not supposed to occur : even if there is twice the same
      --    "import" in a file, it shouldn't be on the same line.
      -- 2. it shouldn't cause any bug, at most a hardly measurable effect on
      --    performance.

   end Add_Unit;

   -- --------------------------------------------------------------------------
   procedure Add_Component (Component         : Component_Attributes;
                            Dependencies      : Dependency_Lists.List) is
      use Component_Maps;
      Key    : constant String := To_Lower (To_String (Component.Name));
      Cursor : Component_Maps.Cursor;
      Tmp_List : Dependency_Lists.List := Dependency_Lists.Empty_List;

   begin
      Put_Debug_Line ("Adding " & Unit_List_Image (Dependencies)
                      & " to component " & To_String (Component.Name));
      -- 1. Relationship list management :
      Relationship_List.Append ((Component, Dependencies));

      -- 2. Dependency list init :
      Cursor := Component_Map.Find (Key);
      -- by convention, key are always stored lower case

      if Component_Maps.Has_Element (Cursor) then
         -- The component is already know
         -- Let's initialize the dependency list with already known dependencies :
         Tmp_List := Component_Maps.Element (Position => Cursor);
         Put_Debug_Line (To_String (Component.Name)
                         & " already known, with dependencies : "
                         & Unit_List_Image (Tmp_List));
      end if;

      -- 3. Owner_Key map management, and Tmp_List building :
      for D of Dependencies loop
         declare
            Owner_Key : constant String := To_String (D.To_Unit);
            C         : constant Owner_Maps.Cursor := Owner_Map.Find (To_Lower (Owner_Key));
            use Owner_Maps;
            use type Dependency_Lists.List;
         begin
            if C = Owner_Maps.No_Element then
               -- normal case, the Unit is not already in a Component
               Dependency_Lists.Append (Tmp_List, D);
               Owner_Map.Insert (To_Lower (Owner_Key), New_Item => (Component.Name, D.File, D.Line));
               Put_Debug_Line ("Adding " &  Owner_Key & " to Owner_Maps");

            else
               declare
                  Dep : constant Dependency := Owner_Maps.Element (C);
               begin
                  IO.Put_Error
                    (Location_Image (D) & To_String (D.To_Unit) & " already in "
                     & To_String (Dep.To_Unit) & " (cf. "
                     & Location_Image (Dep) & "), can't be added to "
                     & To_String (Component.Name));
               end;
            end if;
            -- We don't care if some dependencies are equals, it shouldn't occur.
         end;
      end loop;

      -- 4. Component map management :
      if Component_Maps.Has_Element (Cursor) then
         Component_Map.Replace_Element (Cursor, Tmp_List);
         Put_Debug_Line ("Component " &
                           To_String (Component.Name) & " now depends on " &
                           Unit_List_Image (Tmp_List));
      else
         Component_Map.Insert (Key => Key, New_Item => Tmp_List);
         Put_Debug_Line ("Adding Component " &
                           To_String (Component.Name) & " with dependencies = " &
                           Unit_List_Image (Tmp_List));

      end if;

   end Add_Component;

   -- --------------------------------------------------------------------------
   -- Function: Is_Unit_In_Component
   -- -------------------------------------------------------------------------
   function Is_Unit_In_Component (Unit      : String;
                                  Component : String) return Boolean
   is
      Dep   : Dependency;
      -- Key   : constant String := To_Lower (Component);
      Key   : constant String := To_Lower (Unit);
      -- Found : Boolean := False;
      C     : Owner_Maps.Cursor;
      use Owner_Maps;
      use Ada.Strings;
      -- use Ada.Strings.Fixed;

   begin
      C := Owner_Map.Find (Key);

      -- 4 cases here :
      --   a. the unit is directly claimed by the expected component
      --      => return True
      --   b. the unit is claimed by a component
      --      => recursive call with the found component to check if it is
      --         the owner
      --   c. the unit is a child of one of the owned unit.
      --      Unfortunatly, we have to test all entries to check that.
      --      => if yes, recursive call with the found parent Unit
      --   d. Unit is not claimed by a component
      --      => return Is_A_Child (Unit, Component)

      if C /= No_Element then
         Dep := Element (C);
         if To_Lower (To_String (Dep.To_Unit)) = To_Lower (Component) then
            -- a. the Unit is directly claimed by Dep.To_Unit
            Put_Debug_Line (Unit & " owned by " & To_String (Dep.To_Unit));
            return True;
         else
            -- b. the Unit is claimed by another Component
            Put_Debug_Line ("Recursive call to IUIC (" & To_String (Dep.To_Unit)
                            &  ", " & Component & ")");
            return Is_Unit_In_Component (To_Lower (To_String (Dep.To_Unit)),
                                         Component); --recursive call
         end if;

      else
         for C in Owner_Map.Iterate loop
            if Is_A_Child (Unit, Owner_Maps.Key (C)) then
               -- c. the unit is a child of one of the owned unit.
               Put_Debug_Line
                 (Unit & " is a child of " & Owner_Maps.Key (C)
                  & " that is owned by " & To_String (Dep.To_Unit));
               Put_Debug_Line ("Recursive call to IUIC (" & Owner_Maps.Key (C)
                               & ", " & Component & ")");
               return Is_Unit_In_Component (To_Lower (Owner_Maps.Key (C)),
                                            Component); --recursive call
            end if;
         end loop;

         --   d. Unit is not claimed by a component
         Put_Debug_Line (Unit & " not owned by a component.");
         -- Component is not a component... Fixme: formal parameter name inconsistent
         return Is_A_Child (Unit, Component);

      end if;

   end Is_Unit_In_Component;


end Archicheck.Units;
