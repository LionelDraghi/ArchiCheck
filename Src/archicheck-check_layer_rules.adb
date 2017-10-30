-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Procedure: Archicheck.Check_Layer_Rules body

with Archicheck.IO;
with Archicheck.Dependencies;
with Archicheck.Components;   use Archicheck.Components;
with Archicheck.Settings;

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

procedure Archicheck.Check_Layer_Rules is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Check_Layer_Rules") renames Archicheck.IO.Put_Debug_Line;
   procedure Put_Debug (Msg    : in String  := "";
                        Debug  : in Boolean := Settings.Debug_Mode;
                        Prefix : in String  := "Check_Layer_Rules") renames Archicheck.IO.Put_Debug;

   -- -------------------------------------------------------------------------
   function To_Lower (Source  : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
   begin
      return Translate (Source,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;

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
      Put_Debug_Line ("Is " & Unit & " in " & Component & "? ");

      if Component_Maps.Contains (Component_Map, Component) then
         -- the component was described by one or more declarations
         Units := Component_Maps.Element (Component_Map, Component);
         Found := Unit_Lists.Contains (Units, To_Unbounded_String (Unit));

         Put_Debug ("Is " & Unit & " in " & Unit_List_Image (Units) & "?");
         Put_Debug_Line (Boolean'Image (Found), Prefix => "");

         return Found;

      elsif To_Lower (Head (Unit, Count => Component'Length)) = To_Lower (Component) then
         -- The Unit is a child pkg of the component
         Put_Debug_Line ("YES (child pkg)", Prefix => "");
         return True;

      else
         Put_Debug_Line ("NO", Prefix => "");
         return False;

      end if;
   end Is_Unit_In_Component;


   use Ada.Strings.Unbounded;
   use IO;

begin
   -- for each Layer relationship, we checks each source dependency to see if it's compliant
   for L of Layers loop
      IO.Put_Line ("Checking Layers rules " & To_String (L.Using_Layer) & " -> " & To_String (L.Used_Layer), Only_When_Verbose => True);

      declare
         Client : constant String := To_String (L.Using_Layer);
         Server : constant String := To_String (L.Used_Layer);

      begin
         for D of Dependencies.Get loop
            IO.Put_Line ("Dependency ", Only_When_Verbose => True);

            declare
               X              : constant String := To_String (D.Unit_Name);
               Y              : constant String := To_String (D.Depends_On_Unit);
               Is_Y_In_Client : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Client);
               Is_Y_In_Server : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Server);
               Is_X_In_Client : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Client);
               Is_X_In_Server : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Server);

            begin
               Put_Debug_Line (X & " depends on " & Y);
               Put_Debug_Line ("if " & Y & " is in "
                               & Server & " then " & X
                               & " is     in " & Client);

               if Is_Y_In_Server and not Is_X_In_Client then
                  IO.Put_Error
                    (X & " is not in "
                     & Client & " layer, and so shall not directly use "
                     & Server & " layer");
               end if;

               Put_Debug_Line ("if " & X & " is in "
                               & Server & " then " & Y
                               & " is NOT in " & Client);

               if Is_X_In_Server and Is_Y_In_Client then
                  IO.Put_Error (X & " is in " & Server
                                & " layer, and so shall not use the upper "
                                & Client & " layer");
               end if;

               Put_Debug_Line ("if " & X & " is in "
                               & Client & " then " & Y
                               & " should be in either "
                               & Client & " or " & Server);

               if Is_X_In_Client and not (Is_Y_In_Client or Is_Y_In_Server)
               then
                  IO.Put_Warning (X & " (in " & Client & " layer) uses "
                                  & Y &
                                    " that is neither in the same layer, nor in the lower "
                                  & Server & " layer");
               end if;

            end;

         end loop;

      end;
   end loop;

end Archicheck.Check_Layer_Rules;
