-- -----------------------------------------------------------------------------
-- ArchiCheck: the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Procedure: Archicheck.Rules.Check body
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

-- with Ada.Strings.Fixed;
-- with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

procedure Archicheck.Rules.Check is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Check_Rules") renames Archicheck.IO.Put_Debug_Line;

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
      -- use Ada.Strings.Fixed;

   begin
      -- Put_Debug_Line ("Unit >" & Unit & "<, component >" & Component & "<");
      if Component_Maps.Contains (Component_Map, Component) then
         -- the component was described by one or more declarations
         Units := Component_Maps.Element (Component_Map, Component);
         Found := Unit_Lists.Contains (Units, To_Unbounded_String (Unit));

         if Found then Put_Debug_Line ("Unit " & Unit & " is in the component " & Component); end if;

         return Found;

      else
         return Dependencies.Is_Unit_In (Unit, Component);

      end if;
   end Is_Unit_In_Component;


   use Ada.Strings.Unbounded;
   use IO;

begin
   -- for each Dependancy, we checks each rule to see if the dependancy is compliant
   for D of Dependencies.Get_List loop

      declare
         X              : constant String := To_String (D.From.Name);
         Y              : constant String := To_String (D.To.Name);

      begin
         IO.Put_Line (X & " depends on " & Y, Level => Verbose);

         if Is_Forbidden (D.To.Name) then
            IO.Put_Error ("in file " & To_String (D.From.File) & " : " & Y & " use is forbidden");
            -- no need to further test relationships

         elsif Is_Allowed (D.To.Name) then
            -- target is allowed, no further checks needed
            Put_Debug_Line ("Unit " & To_String (D.To.Name) & " is allowed");

         else

            for R of Rules.Get_List loop
               declare
                  Client         : constant String := To_String (R.Using_Unit);
                  Server         : constant String := To_String (R.Used_Unit);
                  Is_X_In_Client : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Client);
                  Is_X_In_Server : constant Boolean := Is_Unit_In_Component (Unit => X, Component => Server);
                  Is_Y_In_Client : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Client);
                  Is_Y_In_Server : constant Boolean := Is_Unit_In_Component (Unit => Y, Component => Server);

               begin
                  IO.Put_Line ("- Checking relationship " & Relationship_Kind'Image (R.Kind)
                               & " : " & To_String (R.Using_Unit) & " -> "
                               & To_String (R.Used_Unit), Level => Verbose);

                  if (Is_X_In_Server and Is_Y_In_Server) or (Is_X_In_Client and Is_Y_In_Client) then
                     -- no check to do, as both unit are in the same component
                     null;

                  else
                     case R.Kind is
                     when Layer_Over =>
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

                     when Exclusive_Use =>
                        if Is_Y_In_Server and not Is_X_In_Client then
                           IO.Put_Error ("Only " & Client & " is allowed to use "
                                         & Server & ", " & X & " is not");
                        end if;

                     when May_Use =>
                        -- X may use Y actually means that Y shall not use X,
                        -- and this is wath we check here
                        if Is_X_In_Server and Is_Y_In_Client then
                           IO.Put_Error (Client & " is over " & Server & ", so "
                                         & X & " shall not use " & Y);
                        end if;

                     end case;
                  end if;


               end;

            end loop;
         end if;

      end;
   end loop;

end Archicheck.Rules.Check;
