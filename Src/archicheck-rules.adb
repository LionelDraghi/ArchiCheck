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

with Archicheck.IO;
with Archicheck.Units;
with Archicheck.Settings;

with Ada.Strings.Equal_Case_Insensitive;

package body Archicheck.Rules is

   use Ada.Strings.Unbounded;

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Rules")
                             renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   Relationships : Relationship_Lists.List;

   -- --------------------------------------------------------------------------
   function Get_List return Relationship_Lists.List is (Relationships);

   -- --------------------------------------------------------------------------
   procedure Add_Relationship (R : in Relationship) is
   begin
      Relationships.Append (R);
   end Add_Relationship;

   -- --------------------------------------------------------------------------
   package Forbidden_Use_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Unit_Name); -- Forbidden Unit Names
   Forbidden_Use_List : Forbidden_Use_Lists.List;

   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit_Name : in String) return Boolean is
      use Forbidden_Use_Lists;
   begin
      for U of Forbidden_Use_List loop
         if Units.Is_In (Unit_Name, To_String (U)) then
            Put_Debug_Line ("Is_Forbidden (" & Unit_Name & ") return True ("
                            & To_String (U) & " is forbidden)");
            return True;
         end if;
      end loop;
      Put_Debug_Line ("Is_Forbidden (" & Unit_Name & ") return False");
      return False;
      -- Fixme: ajouter une référence à la règle en param out
   end Is_Forbidden;

   -- --------------------------------------------------------------------------
   procedure Add_Forbidden_Unit (Unit_Name : in String) is
   begin
      Forbidden_Use_List.Append (To_Unbounded_String (Unit_Name));
   end Add_Forbidden_Unit;

   -- --------------------------------------------------------------------------
   package Allowed_Use_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Unit_Name); -- Allowed Unit Names
   Allowed_Use_List : Allowed_Use_Lists.List;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit_Name : in String) return Boolean is
      use Allowed_Use_Lists;
   begin
      for U of Allowed_Use_List loop
         if Units.Is_In (Unit_Name, To_String (U)) then
            Put_Debug_Line ("Is_Allowed (" & Unit_Name & ") return True ("
                            & To_String (U) & " is allowed)");
            return True;
         end if;
      end loop;
      Put_Debug_Line ("Is_Allowed (" & Unit_Name & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   procedure Add_Allowed_Unit (Unit_Name : in String) is
   begin
      Allowed_Use_List.Append (To_Unbounded_String (Unit_Name));
   end Add_Allowed_Unit;

   -- --------------------------------------------------------------------------
   -- Function: Is_Allowed
   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit_Name : in String;
                        For_Unit  : in String) return Boolean is
      -- procedure Put_Debug_Line (Msg    : in String  := "";
      --                           Debug  : in Boolean := True;
      --                           Prefix : in String  := "Is_Allowed")
      --                           renames Archicheck.IO.Put_Debug_Line;
   begin
      Put_Debug_Line ("");
      Put_Debug_Line ("Is_Allowed (" & Unit_Name & ", For_Unit => " & For_Unit & ") ?");
      for R of Relationships loop
         Put_Debug_Line ("   Testing R : " & To_String (R.Using_Unit)
                         & " " & Relationship_Kind'Image (R.Kind)
                         & " " & To_String (R.Used_Unit));
         declare
            -- B1 : constant Boolean := R.Kind = May_Use;
            B2 : constant Boolean := Units.Is_In (Unit_Name, To_String (R.Used_Unit));
            B3 : constant Boolean := Units.Is_In (For_Unit,  To_String (R.Using_Unit));
         begin
            -- if B1 then
            Put_Debug_Line ("      Relationship kind : "
                            & Relationship_Kind'Image (R.Kind));
            Put_Debug_Line ("      Is " & Unit_Name & " in "
                            & To_String (R.Used_Unit) & " : " & Boolean'Image (B2));
            Put_Debug_Line ("      Is " & For_Unit & " in "
                            & To_String (R.Using_Unit) & " : " & Boolean'Image (B3));
            -- end if;

            if Units.Is_In (Unit_Name, To_String (R.Used_Unit)) and then
              Units.Is_In (For_Unit,  To_String (R.Using_Unit))
                -- A test on R.Kind is useless, as long as every relation
                -- allow X to use Y.
            then
               Put_Debug_Line ("      Is_Allowed (" & Unit_Name & ", For => "
                               & For_Unit & ") return True (" & To_String (R.Used_Unit)
                               & " is allowed for " & To_String (R.Using_Unit) & ")");
               return True;
            end if;
         end;

      end loop;
      Put_Debug_Line ("Is_Allowed (" & Unit_Name & ", For => "
                      & For_Unit & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   function Allowed_Users (Of_Unit : in String) return Relationship_Lists.List is
      Tmp  : Relationship_Lists.List;
   begin
      for D of Relationships loop
         if Ada.Strings.Equal_Case_Insensitive (To_String (D.Used_Unit), Of_Unit)
         then
            Tmp.Append (D);
         end if;
      end loop;
      return Tmp;
   end Allowed_Users;

   -- --------------------------------------------------------------------------
   function Users_Image (List : in Relationship_Lists.List) return String is
      Tmp : Unbounded_String := Null_Unbounded_String;
      use Relationship_Lists;
   begin
      -- the output is of this kind : "X, Y, Z, and V"
      for C in List.Iterate loop
         if C = List.First then
            Tmp := Element (C).Using_Unit;
         elsif C = List.Last then
            Tmp := Tmp & " and " & Element (C).Using_Unit;
         else
            Tmp := Tmp & ", " & Element (C).Using_Unit;
         end if;
      end loop;
      return (To_String (Tmp));
   end Users_Image;

end Archicheck.Rules;
