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
--
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Settings;

with Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;

package body Archicheck.Units is

   -- Change default Debug parameter value to enable/disable Debug messages
   -- in this package
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line
     (Msg    : in String  := "";
      Debug  : in Boolean := Settings.Debug_Mode; -- change to True to debug
      Prefix : in String  := "Units") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   function "+" (Name : Unit_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return Unit_Name is
     (Unit_Name'(To_Unbounded_String (Name)));
   function To_US (Name : Unit_Name) return Unbounded_String is
     (Unbounded_String (Name));
   function "+" (Name : Unbounded_String) return Unit_Name is
     (Unit_Name (Name));

   function Hash_Case_Insensitive
     (Key : Unit_Name) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (+Key));

   -- --------------------------------------------------------------------------
   Component_Map : Component_Maps.Map;
   function Get_Component_Map return Component_Maps.Map is
     (Component_Map);

   -- --------------------------------------------------------------------------
   -- Owner map run the other way round of Component_Maps : the key is a Unit
   -- that is contained in a component, and the Element_Type is this component.
   -- Note that this is not a list, as a Unit is not supposed to be in more
   -- Component.
   -- Note that it is also maintained in parallel with the Dependency_List.
   package Owner_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unit_Name,
      Element_Type    => Dependency_Target,
      Hash            => Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => "=");
   Owner_Map     : Owner_Maps.Map;

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
   procedure Dump is
      use Archicheck.IO;
      use Sources;
   begin
      for D of Dependency_List loop
         for T of D.Targets loop
            Put_Line ((+D.Source.Name)
                      & " " & Unit_Description (D.Source)
                      & " depends on "
                      & (+T.To_Unit),
                      Level => Quiet);
            -- gives file details only when verbose :
            Put_Line ("   according to " & Location_Image (T.Location),
                      Level => Verbose);
         end loop;
      end loop;

   end Dump;

   -- --------------------------------------------------------------------------
   procedure Add_Unit (Unit    : Unit_Attributes;
                       Targets : Dependency_Targets.List) is
   begin
      pragma Assert (Unit not in Component_Attributes,
                     "Call Add_Component instead");
      Put_Debug_Line ("Adding dependencies " & Unit_List_Image (Targets)
                      & " to unit " & To_String (Unit.Name));

      Dependency_List.Append ((Unit, Targets));
      -- We do not verify that some of those dependencies are not already
      -- registered, as :
      -- 1. this is not supposed to occur : even if there is twice the same
      --    "import" in a file, it shouldn't be on the same line.
      -- 2. it shouldn't cause any bug, and at most a hardly measurable effect
      --    on performance.

   end Add_Unit;

   -- --------------------------------------------------------------------------
   procedure Add_Component (Component : Component_Attributes;
                            Targets   : Dependency_Targets.List) is
      use Component_Maps;
      Key      : constant Unit_Name := Component.Name;
      Cursor   : Component_Maps.Cursor;
      Tmp_List : Dependency_Targets.List := Dependency_Targets.Empty_List;

   begin
      Put_Debug_Line ("Adding " & Unit_List_Image (Targets)
                      & " to component " & To_String (Component.Name));
      -- 1. Relationship list management :
      Dependency_List.Append ((Component, Targets));

      -- 2. Dependency list init :
      Cursor := Component_Map.Find (Key);

      if Component_Maps.Has_Element (Cursor) then
         -- The component is already known
         -- Initializing the dependency list with already known dependencies :
         Tmp_List := Component_Maps.Element (Position => Cursor);
         Put_Debug_Line (To_String (Component.Name)
                         & " already known, with dependencies : "
                         & Unit_List_Image (Tmp_List));
      end if;

      -- 3. Owner_Key map management, and Tmp_List building :
      for D of Targets loop
         declare
            Owner : constant Unit_Name := D.To_Unit;
            C     : constant Owner_Maps.Cursor := Owner_Map.Find (Owner);
            use Owner_Maps;
            use Sources;
         begin
            if C = Owner_Maps.No_Element then
               -- normal case, the Unit is not already in a Component
               Dependency_Targets.Append (Tmp_List, D);
               Owner_Map.Insert (Owner,
                                 New_Item => (Component.Name, D.Location));
               Put_Debug_Line ("Adding " &  (+Owner) & " to Owner_Maps");

            else
               declare
                  Dep : constant Dependency_Target := Owner_Maps.Element (C);
               begin
                  IO.Put_Error
                    (Location_Image (D.Location) & To_String (D.To_Unit)
                     & " already in " & To_String (Dep.To_Unit) & " (cf. "
                     & Location_Image (Dep.Location) & "), can't be added to "
                     & To_String (Component.Name));
               end;
            end if;
            -- We don't care if some dependencies are equals, it shouldn't occur
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
         Put_Debug_Line ("Adding Component "
                         & To_String (Component.Name) & " with dependencies = "
                         & Unit_List_Image (Tmp_List));

      end if;

   end Add_Component;

   -- --------------------------------------------------------------------------
   function Is_A_Component (Unit : Unit_Name) return Boolean is
     (Component_Map.Contains (Unit));

   -- --------------------------------------------------------------------------
   function Is_A_Child (Child  : Unit_Name;
                        Parent : Unit_Name) return Boolean
   is
      C : constant String := +Child;
      P : constant String := +Parent;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      -- procedure Put_Debug_Line
      --   (Msg    : in String  := "";
      --    Debug  : in Boolean := True;
      --    Prefix : in String  := "Is_A_Child")
      --    renames Archicheck.IO.Put_Debug_Line;

   begin
      if Ada.Strings.Equal_Case_Insensitive (C, P) then
         -- The Unit is the component
         Put_Debug_Line ("Unit " & C & " = " & P);
         return True;

      elsif C'Length > P'Length and then
        (Ada.Strings.Equal_Case_Insensitive
           (Head (C, Count => P'Length), P) and C (P'Length + 1) = '.')
      then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("Unit " & C & " is a child of " & P);
         return True;

      else
         Put_Debug_Line ("Unit " & C & " not a child of " & P, Prefix => "");
         return False;

      end if;
   end Is_A_Child;

   -- --------------------------------------------------------------------------
   function Is_In (Unit    : Unit_Name;
                   In_Unit : Unit_Name) return Boolean
   is
      Dep : Dependency_Target;
      Key : constant Unit_Name := Unit;
      C   : Owner_Maps.Cursor;
      use Owner_Maps;
      use Ada.Strings;

   begin
      -- 5 cases here :
      --   a. the unit IS the component, or a child of
      --      => return True
      --   b. the unit is directly claimed by the expected component
      --      => return True
      --   c. the unit is claimed by a component
      --      => recursive call with the found component to check if it is
      --         the owner
      --   d. the unit is a child of one of the owned unit.
      --      Unfortunatly, we have to test all entries to check that.
      --      => if yes, recursive call with the found parent Unit
      --   e. Unit is not claimed by a component
      --      => return False

      if Is_A_Child (Child  => Unit, Parent => In_Unit) then
         --   a. the unit IS the component, or a child of
         return True;
      end if;

      C := Owner_Map.Find (Key);

      if C /= Owner_Maps.No_Element then
         Dep := Element (C);
         if Dep.To_Unit = In_Unit then
            -- b. the Unit is directly claimed by Dep.To_Unit
            Put_Debug_Line ((+Unit) & " owned by " & (+Dep.To_Unit));
            return True;
         else
            -- c. the Unit is claimed by another Component
            Put_Debug_Line ("1 Recursive call to IUIC (" & (+Dep.To_Unit)
                            &  ", " & (+In_Unit) & ")");
            return Is_In (Dep.To_Unit, In_Unit); --recursive call
         end if;

      else
         for C in Owner_Map.Iterate loop
            if Is_A_Child (Unit, Owner_Maps.Key (C)) then
               -- d. the unit is a child of one of the owned unit.
               Put_Debug_Line
                 ((+Unit) & " is a child of " & (+Owner_Maps.Key (C))
                  & " that is owned by " & (+Dep.To_Unit));
               Put_Debug_Line ("2 Recursive call to IUIC ("
                               & (+Owner_Maps.Key (C)) & ", "
                               & (+In_Unit) & ")");
               return Is_In (Owner_Maps.Key (C), In_Unit); --recursive call
            end if;
         end loop;

         --   e. Unit is not claimed by a component
         Put_Debug_Line ((+Unit) & " not owned by a component.");
         return False;

      end if;

   end Is_In;

   -- --------------------------------------------------------------------------
   function Is_Involved_In_A_Component (Unit : in Unit_Name) return Boolean is
   begin
      for C in Owner_Map.Iterate loop
         if Is_In (Unit, In_Unit => Owner_Maps.Key (C)) then
            return True;
         end if;
      end loop;
      for C in Component_Map.Iterate loop
         if Is_In (Unit, In_Unit => Component_Maps.Key (C)) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Involved_In_A_Component;

end Archicheck.Units;
