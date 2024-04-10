-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Sources;
with Archicheck.Settings;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;

procedure Archicheck.Rules.Check_Unrelated_Rules_Units is

   procedure Put_Debug_Line
     (Msg    : in String  := "";
      Debug  : in Boolean := Settings.Debug_Mode; -- change to True to debug
      Prefix : in String  := "") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   function Match_A_Compilation_Unit (Unit : Unit_Name) return Boolean is
   begin
      Put_Debug_Line ("Matching " & (+Unit) & "...");
      for D of Dependency_List loop
         if D.Source.Kind in Compilation_Unit then
            Put_Debug_Line ("Is_A_Child (" & (+D.Source.Name)
                            & ", Parent => " & (+Unit));
            if Is_A_Child (D.Source.Name, Parent => Unit) then
               --  Put_Debug_Line ("Match_A_Comp_Unit " & (+Unit)
               --                  & " return True");
               return True;
            end if;
            for T of D.Targets loop
               Put_Debug_Line ("Is_A_Child (" & (+T.To_Unit)
                               & ", Parent => " & (+Unit));
               if Is_A_Child (T.To_Unit, Parent => Unit) then
                  --  Put_Debug_Line ("Match_A_Comp_Unit " & (+Unit)
                  --                  & " return True");
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      Put_Debug_Line ("Match_A_Comp_Unit " & (+Unit) & " return False");
      return False;
   end Match_A_Compilation_Unit;

   -- --------------------------------------------------------------------------
   procedure Put_Warning_Msg (Unit : Unit_Name;
                              Loc  : Sources.Location) is
      use Archicheck.IO;
      use Sources;
   begin
      Put_Warning (Location_Image (Loc) & (+Unit)
                   & " do not match any compilation unit");
   end Put_Warning_Msg;

   -- --------------------------------------------------------------------------
   -- Fixme: Ugly solution to manage a unique item list
   function Hash_Case_Insensitive
     (Key : Unit_Name) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (+Key));
   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unit_Name,
      Element_Type    => Sources.Location,
      Hash            => Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => Sources."=");
   Units_In_Rules_Map : Maps.Map;
   -- Stores each unit in rules file, including those in component
   -- declarations, but :
   -- - NOT Components (no matching sources by definition);
   -- - and NOT forbidden units : we won't warn users if there is no
   --   matching source, as this is the normal situation.

   -- --------------------------------------------------------------------------
   procedure Add_Unit (Name : Unit_Name; Loc : Sources.Location) is
   begin
      if not Is_A_Component (Name) then
         if not Units_In_Rules_Map.Contains (Name) then
            Units_In_Rules_Map.Insert (Name, Loc);
         end if;
      end if;
   end Add_Unit;

begin
   -- A. First we store each unit in rules file, including those in
   --    component declarations, but :
   --    - NOT Components (no matching sources by definition);
   --    - and NOT forbidden units : we won't warn users if there is no
   --      matching source, as this is the normal situation.

   -- A.1. check units in "Subject only" rules
   for R of Get_Subject_Only_Rule_List loop
      if R.Kind /= Forbidden_Use then
         Add_Unit (R.Subject_Unit, R.Location);
      end if;
   end loop;

   -- A.2. check units in "Subject & Object" rules
   for R of Get_With_Object_Rule_List loop
      Add_Unit (R.Subject_Unit, R.Location);
      Add_Unit (R.Object_Unit, R.Location);
   end loop;

   -- A.3. check units in Component declarations
   for C of Units.Get_Component_Map loop
      for D of C loop
         Add_Unit (D.To_Unit, D.Location);
      end loop;
   end loop;

   -- B. Then check each Unit in the against Sources
   for C in Units_In_Rules_Map.Iterate loop
      if not Match_A_Compilation_Unit (Maps.Key (C)) then
         Put_Warning_Msg (Maps.Key (C), Maps.Element (C));
      end if;
   end loop;

end Archicheck.Rules.Check_Unrelated_Rules_Units;
