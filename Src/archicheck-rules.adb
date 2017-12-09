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
   begin
      return Forbidden_Use_List.Contains (Unit);
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
         if Dependencies.Is_Unit_In (To_String (Unit), To_String (U)) then
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

end Archicheck.Rules;
