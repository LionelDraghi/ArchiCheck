-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
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

with Archicheck.IO;                      use Archicheck.IO;
with Archicheck.Settings;

with Ada.Strings.Equal_Case_Insensitive;

package body Archicheck.Rules is

   -- Change default Debug parameter value to enable/disable
   -- Debug messages in this package
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Rules")
                             renames Archicheck.IO.Put_Debug_Line;

   -- --------------------------------------------------------------------------
   Subject_Only_Rule_List, With_Object_Rule_List : Rule_Lists.List;

   -- --------------------------------------------------------------------------
   function Get_With_Object_Rule_List return Rule_Lists.List is
     (With_Object_Rule_List);
   function Get_Subject_Only_Rule_List return Rule_Lists.List is
     (Subject_Only_Rule_List);

   -- --------------------------------------------------------------------------
   procedure Print_Rule (R : in Rule) is
      use Sources;
   begin
      case R.Kind is
         when Allowed_Use =>
            Put_Line (Location_Image (R.Location)
                      & "Use of " & (+R.Subject_Unit) & " allowed ",
                      Level => IO.Quiet);

         when Are_Independent =>
            Put_Line (Location_Image (R.Location)
                      & (+R.Subject_Unit) & " and " & (+R.Object_Unit) & " are independent",
                      Level => IO.Quiet);

         when Exclusive_Use =>
            Put_Line (Location_Image (R.Location)
                      & "Only " & (+R.Subject_Unit) & " may use "
                      & (+R.Object_Unit),
                      Level => IO.Quiet);

         when Forbidden_Use =>
            Put_Line (Location_Image (R.Location)
                      & "Use of " & (+R.Subject_Unit) & " is forbidden",
                      Level => IO.Quiet);

         when Layer_Over =>
            Put_Line (Location_Image (R.Location)
                      & "Layer " & (+R.Subject_Unit)
                      & " is over layer " & (+R.Object_Unit),
                      Level => IO.Quiet);

         when May_Use =>
            Put_Line (Location_Image (R.Location)
                      & (+R.Subject_Unit) & " may use "
                      & (+R.Object_Unit),
                      Level => IO.Quiet);

      end case;
   end Print_Rule;

   -- --------------------------------------------------------------------------
   procedure Add_Rule (R : in Rule) is
   begin
      if R.Kind in No_Object_Rule_Kind then
         Subject_Only_Rule_List.Append (R);
      else
         With_Object_Rule_List.Append (R);
      end if;

      if Settings.List_Rules then Print_Rule (R); end if;

   end Add_Rule;

   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit : in Unit_Name) return Boolean is
   begin
      for R of Subject_Only_Rule_List loop
         declare
            Subject : constant Unit_Name := R.Subject_Unit;
         begin
            if R.Kind = Forbidden_Use and Units.Is_In (Unit, Subject) then
               Put_Debug_Line ("Is_Forbidden (" & (+Unit) & ") return True ("
                               & (+Subject) & " is forbidden)");
               return True;
            end if;
         end;
      end loop;
      Put_Debug_Line ("Is_Forbidden (" & (+Unit) & ") return False");
      return False;
      -- Fixme: ajouter une référence à la règle en param out
   end Is_Forbidden;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit : in Unit_Name) return Boolean is
   begin
      for R of Subject_Only_Rule_List loop
         declare
            Subject : constant Unit_Name := R.Subject_Unit;
         begin
            if R.Kind = Allowed_Use and Units.Is_In (Unit, Subject) then
               Put_Debug_Line ("Is_Allowed (" & (+Unit) & ") return True ("
                               & (+Subject) & " is allowed)");
               return True;
            end if;
         end;
      end loop;
      Put_Debug_Line ("Is_Allowed (" & (+Unit) & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit     : in Unit_Name;
                        For_Unit : in Unit_Name) return Boolean is
      -- procedure Put_Debug_Line (Msg    : in String  := "";
      --                           Debug  : in Boolean := True;
      --                           Prefix : in String  := "Is_Allowed")
      --                           renames Archicheck.IO.Put_Debug_Line;
   begin
      for R of With_Object_Rule_List loop
         declare
            Object  : constant Unit_Name := R.Object_Unit;
            Subject : constant Unit_Name := R.Subject_Unit;
         begin
            if Units.Is_In (Unit, Object) and then
              Units.Is_In (For_Unit, Subject)
              -- A test on R.Kind is useless here, as every rule
              -- with an Object_Unit allows Subject_Unit to use Object_Unit.
            then
               Put_Debug_Line ("Is_Allowed (" & (+Unit) & ", For => "
                               & (+For_Unit) & ") return True (" & (+Object)
                               & " is allowed for " & (+Subject) & ")");
               return True;
            end if;
         end;

      end loop;
      Put_Debug_Line ("Is_Allowed (" & (+Unit) & ", For => "
                      & (+For_Unit) & ") return False");
      return False;
   end Is_Allowed;

   -- --------------------------------------------------------------------------
   function Allowed_Users (Of_Unit : in Unit_Name) return Rule_Lists.List is
      Tmp : Rule_Lists.List;
   begin
      for R of With_Object_Rule_List loop
         if Ada.Strings.Equal_Case_Insensitive (To_String (R.Object_Unit),
                                                +Of_Unit)
         then
            Tmp.Append (R);
         end if;
      end loop;
      return Tmp;
   end Allowed_Users;

   -- --------------------------------------------------------------------------
   function Is_Involved_In_A_Rule (Unit : in Unit_Name) return Boolean is
   begin
      IO.Put_Line ("", Level => Verbose);
      IO.Put_Line ("-----------------", Level => Verbose);
      for R of Subject_Only_Rule_List loop
         if Is_In (Unit, In_Unit => R.Subject_Unit) then
            IO.Put_Line (Item  => "+ " & (+Unit) & " Involved in "
                         & (+R.Subject_Unit),
                         Level => Verbose);
            return True;
         end if;
         IO.Put_Line (Item  => "- " & (+Unit) & " not Involved in "
                      & (+R.Subject_Unit),
                      Level => Verbose);
      end loop;
      for R of With_Object_Rule_List loop
         if Is_In (Unit, In_Unit => R.Subject_Unit)
           or else Is_In (Unit, In_Unit => R.Object_Unit)
         then
            IO.Put_Line (Item  => "+ " & (+Unit) & " Involved in "
                         & (+R.Subject_Unit) & " or "
                         & (+R.Object_Unit),
                         Level => Verbose);
            return True;
         end if;
         IO.Put_Line (Item  => "- " & (+Unit) & " not Involved in "
                      & (+R.Subject_Unit) & " or "
                      & (+R.Object_Unit),
                      Level => Verbose);
      end loop;
      return False;
   end Is_Involved_In_A_Rule;


end Archicheck.Rules;
