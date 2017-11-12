-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Archicheck.Check_Layer_Rules body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Archicheck.IO;
with Archicheck.Dependencies;
with Archicheck.Components;   use Archicheck.Components;
with Archicheck.Settings;
with Archicheck.Layers;

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

procedure Archicheck.Check_Layer_Rules is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Check_Layer_Rules") renames Archicheck.IO.Put_Debug_Line;
--     procedure Put_Debug (Msg    : in String  := "";
--                          Debug  : in Boolean := Settings.Debug_Mode;
--                          Prefix : in String  := "Check_Layer_Rules") renames Archicheck.IO.Put_Debug;

   -- --------------------------------------------------------------------------
   -- Function: To_Lower
   -- -------------------------------------------------------------------------
   function To_Lower (Source  : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
   begin
      return Translate (Source,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;

   -- --------------------------------------------------------------------------
   -- Function: Is_Unit_In_Component
   -- -------------------------------------------------------------------------
   function Is_Unit_In_Component (Unit      : String;
                                  Component : String) return Boolean
   is
      use Ada.Strings.Unbounded;
      Units : Unit_Lists.List;
      Found : Boolean := False;
      use Ada.Strings;
      use Ada.Strings.Fixed;

   begin
      if Component_Maps.Contains (Component_Map, Component) then
         -- the component was described by one or more declarations
         Units := Component_Maps.Element (Component_Map, Component);
         Found := Unit_Lists.Contains (Units, To_Unbounded_String (Unit));

         if Found then Put_Debug_Line ("Unit " & Unit & " is in the component " & Component); end if;

         return Found;

      elsif To_Lower (Unit) = To_Lower (Component) then
         -- The Unit is the component
         Put_Debug_Line ("Unit " & Unit & " is (in) the component " & Component);
         return True;

      elsif Unit'Length > Component'Length and then (To_Lower (Head (Unit, Count => Component'Length)) = To_Lower (Component) and Unit (Component'Length + 1) = '.') then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("Unit " & Unit & " is (as child) in the component " & Component);
         return True;

      else
         Put_Debug_Line ("Unit " & Unit & " not in component " & Component, Prefix => "");
         return False;

      end if;
   end Is_Unit_In_Component;


   use Ada.Strings.Unbounded;
   use IO;

begin
   -- for each Layer relationship, we checks each source dependency to see if it's compliant
   for L of Layers.Get_List loop
      IO.Put_Line ("Checking Layers rules " & To_String (L.Using_Layer) & " -> " & To_String (L.Used_Layer), Only_When_Verbose => True);

      declare
         Client : constant String := To_String (L.Using_Layer);
         Server : constant String := To_String (L.Used_Layer);

      begin
         for D of Dependencies.Get_List loop
            declare
               X              : constant String := To_String (D.From.Name);
               Y              : constant String := To_String (D.To.Name);
               Is_X_In_Client : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Client);
               Is_X_In_Server : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Server);
               Is_Y_In_Client : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Client);
               Is_Y_In_Server : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Server);

            begin
               IO.Put_Line ("   - " & X & " depends on " & Y, Only_When_Verbose => True);

               if (Is_X_In_Server and Is_Y_In_Server) or (Is_X_In_Client and Is_Y_In_Client) then
                  -- no check to do, as both unit are in the same layer
                  null;

               else
                  if Is_Y_In_Server and not Is_X_In_Client then
                     IO.Put_Warning (X & " is neither in "
                                     & Client & " or " & Server & " layer, and so shall not directly use "
                                     & Y & " in the "
                                     & Server & " layer");
                  end if;

                  if Is_X_In_Server and Is_Y_In_Client then
                     IO.Put_Error (X & " is in " & Server
                                   & " layer, and so shall not use "
                                   & Y & " in the upper "
                                   & Client & " layer");
                  end if;

                  if Is_X_In_Client and not (Is_Y_In_Client or Is_Y_In_Server)
                  then
                     IO.Put_Warning (X & " (in " & Client & " layer) uses "
                                     & Y & " that is neither in the same layer, nor in the lower "
                                     & Server & " layer");
                  end if;
               end if;

            end;

         end loop;

      end;
   end loop;

end Archicheck.Check_Layer_Rules;
