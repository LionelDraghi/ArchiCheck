-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Rules body
--
-- Purpose:
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Archicheck.Dependencies;
with Archicheck.Settings;
with Archicheck.IO;
with Archicheck.Components;

package body Archicheck.Rules is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Rules") renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   --     package Relationship_Lists is
   --       new Ada.Containers.Doubly_Linked_Lists (Relationship_Kind);
   Relationships : Relationship_Lists.List;

   -- --------------------------------------------------------------------------
   function Get_List return Relationship_Lists.List is (Relationships);

   -- --------------------------------------------------------------------------
   procedure Add_Relationship (R : in Relationship) is
   begin
      Relationships.Append (R);
   end Add_Relationship;


   -- --------------------------------------------------------------------------
   use type Ada.Strings.Unbounded.Unbounded_String;
   package Forbidden_Use_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Unit_Name); -- Forbidden Unit Names
   Forbidden_Use_List : Forbidden_Use_Lists.List;

   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit : in Unit_Name) return Boolean is
      use Forbidden_Use_Lists;
      use Ada.Strings.Unbounded;
   begin
      for U of Forbidden_Use_List loop
         if Is_Unit_In_Component (To_String (Unit), To_String (U)) then
            Put_Debug_Line ("Is_Forbidden (" & To_String (Unit) & ") return True (" & To_String (U) & " is forbidden)");
            return True;
         end if;
      end loop;
      Put_Debug_Line ("Is_Forbidden (" & To_String (Unit) & ") return False");
      return False;
   end Is_Forbidden;

   -- --------------------------------------------------------------------------
   procedure Add_Forbidden_Unit (Unit : in Unit_Name) is
   begin
      Forbidden_Use_List.Append (Unit);
   end Add_Forbidden_Unit;


   -- --------------------------------------------------------------------------
   package Allowed_Use_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Unit_Name); -- Allowed Unit Names
   Allowed_Use_List : Allowed_Use_Lists.List;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit : in Unit_Name) return Boolean is
      use Allowed_Use_Lists;
      use Ada.Strings.Unbounded;
   begin
      for U of Allowed_Use_List loop
         if Is_Unit_In_Component (To_String (Unit), To_String (U)) then
               Put_Debug_Line ("Is_Allowed (" & To_String (Unit) & ") return True (" & To_String (U) & " is allowed)");
            return True;
         end if;
      end loop;
      Put_Debug_Line ("Is_Allowed (" & To_String (Unit) & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit (Unit : in Unit_Name) is
   begin
      Allowed_Use_List.Append (Unit);
   end Add_Allowed_Unit;

   -- --------------------------------------------------------------------------
   -- Function: Is_Unit_In_Component
   -- -------------------------------------------------------------------------
   function Is_Unit_In_Component (Unit      : String;
                                  Component : String) return Boolean
   is
      use Ada.Strings.Unbounded;
      use Archicheck.Components;
      Units : Unit_Lists.List;
      Found : Boolean := False;
      use Ada.Strings;
      -- use Ada.Strings.Fixed;

   begin
      if Component_Maps.Contains (Component_Map, Component) then
         -- Put_Debug (" (component known) ");

         -- the component was described by one or more declarations
         Units := Component_Maps.Element (Component_Map, Component);
         for U of Units loop
            Put_Debug_Line ("Is " & Unit & " in " & To_String (U) & " defined by Component " & Component);
            Found := Dependencies.Is_Unit_In (Unit, To_String (U));
            Put_Debug_Line ("Found " & Boolean'Image (Found));
            exit when Found;
         end loop;
         -- Found := Unit_Lists.Contains (Units, To_Unbounded_String (Unit));

      else
         Found := Dependencies.Is_Unit_In (Unit, Component);

      end if;

      if Found then
         Put_Debug_Line ("Unit >" & Unit & "< is in component >" & Component & "< ");
      else
         Put_Debug_Line ("Unit >" & Unit & "< is NOT in component >" & Component & "< ");
      end if;

      return Found;

   end Is_Unit_In_Component;


end Archicheck.Rules;
