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
   Subject_Only_Rule_List, With_Object_Rule_List : Rule_Lists.List;

   -- --------------------------------------------------------------------------
   function Get_With_Object_Rule_List return Rule_Lists.List is
     (With_Object_Rule_List);

   -- --------------------------------------------------------------------------
   procedure Add_Rule (R : in Rule) is
   begin
      if R.Kind in No_Object_Rule_Kind then
         Subject_Only_Rule_List.Append (R);
      else
         With_Object_Rule_List.Append (R);
      end if;
   end Add_Rule;

   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit_Name : in String) return Boolean is
   begin
      for R of Subject_Only_Rule_List loop
         declare
            Subject : constant String := To_String (R.Subject_Unit);
         begin
            if R.Kind = Forbidden_Use and Units.Is_In (Unit_Name, Subject) then
               Put_Debug_Line ("Is_Forbidden (" & Unit_Name & ") return True ("
                               & Subject & " is forbidden)");
               return True;
            end if;
         end;
      end loop;
      Put_Debug_Line ("Is_Forbidden (" & Unit_Name & ") return False");
      return False;
      -- Fixme: ajouter une référence à la règle en param out
   end Is_Forbidden;

--     -- --------------------------------------------------------------------------
--     procedure Add_Forbidden_Unit (Unit_Name : in String) is
--     begin
--        Subject_Only_Rule_List.Append
--          ((Kind         => Forbidden_Use,
--            Subject_Unit => To_Unbounded_String (Unit_Name)));
--     end Add_Forbidden_Unit;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit_Name : in String) return Boolean is
      -- use Allowed_Use_Lists;
   begin
      for R of Subject_Only_Rule_List loop
         declare
            Subject : constant String := To_String (R.Subject_Unit);
         begin
            if R.Kind = Allowed_Use and Units.Is_In (Unit_Name, Subject) then
               Put_Debug_Line ("Is_Allowed (" & Unit_Name & ") return True ("
                               & Subject & " is allowed)");
               return True;
            end if;
         end;
      end loop;
      Put_Debug_Line ("Is_Allowed (" & Unit_Name & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit_Name : in String;
                        For_Unit  : in String) return Boolean is
      -- procedure Put_Debug_Line (Msg    : in String  := "";
      --                           Debug  : in Boolean := True;
      --                           Prefix : in String  := "Is_Allowed")
      --                           renames Archicheck.IO.Put_Debug_Line;
   begin
      for R of With_Object_Rule_List loop
         declare
            Object  : constant String := To_String (R.Object_Unit);
            Subject : constant String := To_String (R.Subject_Unit);
         begin
            if Units.Is_In (Unit_Name, Object) and then
              Units.Is_In (For_Unit, Subject)
              -- A test on R.Kind is useless here, as every rule
              -- with an Object_Unit allows Subject_Unit to use Object_Unit.
            then
               Put_Debug_Line ("Is_Allowed (" & Unit_Name & ", For => "
                               & For_Unit & ") return True (" & Object
                               & " is allowed for " & Subject & ")");
               return True;
            end if;
         end;

      end loop;
      Put_Debug_Line ("Is_Allowed (" & Unit_Name & ", For => "
                      & For_Unit & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   function Allowed_Users (Of_Unit : in String) return Rule_Lists.List is
      Tmp  : Rule_Lists.List;
   begin
      for R of With_Object_Rule_List loop
         if Ada.Strings.Equal_Case_Insensitive (To_String (R.Object_Unit), Of_Unit)
         then
            Tmp.Append (R);
         end if;
      end loop;
      return Tmp;
   end Allowed_Users;

   -- --------------------------------------------------------------------------
   function Users_Image (List : in Rule_Lists.List) return String is
      Tmp : Unbounded_String := Null_Unbounded_String;
      use Rule_Lists;
   begin
      -- the output is of this kind : "X, Y, Z, and V"
      for C in List.Iterate loop
         if C = List.First then
            Tmp := Element (C).Subject_Unit;
         elsif C = List.Last then
            Tmp := Tmp & " and " & Element (C).Subject_Unit;
         else
            Tmp := Tmp & ", " & Element (C).Subject_Unit;
         end if;
      end loop;
      return (To_String (Tmp));
   end Users_Image;

end Archicheck.Rules;
