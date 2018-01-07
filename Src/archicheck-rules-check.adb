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
with Archicheck.Units;
with Archicheck.Settings;

procedure Archicheck.Rules.Check is

   -- Change default Debug parameter value to enable/disable
   -- Debug messages in this package
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := Settings.Debug_Mode;
                             Prefix : in String  := "Check_Rules")
                             renames Archicheck.IO.Put_Debug_Line;
   --     procedure Put_Debug      (Msg    : in String  := "";
   --                               Debug  : in Boolean := Settings.Debug_Mode;
   --                               Prefix : in String  := "")
   --                               renames Archicheck.IO.Put_Debug;

   use IO;
   use Sources;

begin
   -- for each Dependency, we checks each rule to see if the dependency
   -- is compliant
   for Dep of Units.Dependency_List loop
      for Target of Dep.Targets loop

         declare
            From : constant Unit_Name := Dep.Source.Name;
            To   : constant Unit_Name := Target.To_Unit;

         begin
            IO.Put_Line (Location_Image (Target.Location) & "checking "
                         & (+From) & " dependence on " & (+To),
                         Level => Verbose);

            -- Order is important in the following tests.
            -- As an explicit permission has precedence on an explicit
            -- interdiction, so Is_Allowed is tested first

            if Is_Allowed (To) then
               -- 1. First test if the target use is OK, independently
               --    of the caller.

               Put_Debug_Line ("Unit " & (+To) & " is allowed");
               -- More details if verbose :
               -- Fixme: il faut que Is_Allowed et les autres retourne un
               -- Rule_Location pour pouvoir avoir un message helpful
               -- IO.Put_Line ("   according to " & Location_Image (D),
               --             Level => Verbose);
               -- target is allowed, no further checks needed

            elsif Is_Allowed (To, For_Unit => From) then
               -- 2. Test if the target use is OK for this specific caller.
               Put_Debug_Line ("Unit " & (+To) & " is allowed for " & (+From));

            elsif Is_Forbidden (To) then
               -- 3. Test if the target is forbidden.
               --    And if so, no need to further test relationships

               IO.Put_Error (Location_Image (Target.Location) & (+To)
                             & " use is forbidden");
               -- More details if verbose :
               -- IO.Put_Line ("   according to " & Location_Image (D),
               --             Level => Verbose);

            else
               -- 4. Let's test other rules
               for Rule of Rules.Get_With_Object_Rule_List loop
                  declare
                     Client         : constant Unit_Name := Rule.Subject_Unit;
                     Server         : constant Unit_Name := Rule.Object_Unit;
                     Is_X_In_Client : constant Boolean := Units.Is_In
                       (Unit => From, In_Unit => Client);
                     Is_X_In_Server : constant Boolean := Units.Is_In
                       (Unit => From, In_Unit => Server);
                     Is_Y_In_Client : constant Boolean := Units.Is_In
                       (Unit => To, In_Unit => Client);
                     Is_Y_In_Server : constant Boolean := Units.Is_In
                       (Unit => To, In_Unit => Server);

                  begin
                     IO.Put_Line ("- Checking relationship "
                                  & Rule_Kind'Image (Rule.Kind) & " : "
                                  & To_String (Rule.Subject_Unit) & " -> "
                                  & To_String (Rule.Object_Unit),
                                  Level => Verbose);

                     if (Is_X_In_Server and Is_Y_In_Server) or
                       (Is_X_In_Client and Is_Y_In_Client)
                     then
                        -- no check to do, as both unit are in the same component
                        null;

                     else
                        case Rule.Kind is
                           when Layer_Over =>
                              -- Error first, and if there is an error,
                              -- following check should be useless
                              if Is_X_In_Server and Is_Y_In_Client then
                                 IO.Put_Error (Location_Image (Target.Location) & (+From) & " is in " & (+Server)
                                               & " layer, and so shall not use "
                                               & (+To) & " in the upper "
                                               & (+Client) & " layer");

                              elsif Is_X_In_Client and not (Is_Y_In_Client or Is_Y_In_Server) then
                                 if Is_Allowed (From, To) then
                                    null;
                                    -- This error case (lower using upper) is already reported in the previous "if"
                                 else
                                    IO.Put_Warning (Location_Image (Target.Location) & (+From) & " (in " & (+Client) & " layer) uses "
                                                    & (+To) & " that is neither in the same layer, nor in the lower "
                                                    & (+Server) & " layer");
                                 end if;

                              elsif Is_Y_In_Server and not Is_X_In_Client then
                                 IO.Put_Warning (Location_Image (Target.Location) & (+From) & " is neither in "
                                                 & (+Client) & " or " & (+Server) & " layer, and so shall not directly use "
                                                 & (+To) & " in the "
                                                 & (+Server) & " layer");
                              end if;

                           when Exclusive_Use =>
                              if Is_Y_In_Server and not Is_X_In_Client then
                                 declare
                                    use type Ada.Containers.Count_Type;
                                    Users : constant Rule_Lists.List := Allowed_Users (Server);
                                    Verb  : constant String := (if Users.Length = 1 then " is " else " are ");

                                 begin
                                    if not Is_Allowed (Client, Server) then
                                       IO.Put_Error (Location_Image (Target.Location) & "Only " & Users_Image (Users)
                                                     & Verb & "allowed to use "
                                                     & (+Server) & ", " & (+From) & " is not");
                                    else
                                       IO.Put_Error ((+Client) & " is allowed to use " & (+Server));
                                    end if;
                                 end;
                              end if;

                           when May_Use =>
                              -- X may use Y actually means that Y shall not use X,
                              -- and this is what we check here
                              if Is_X_In_Server and Is_Y_In_Client then
                                 IO.Put_Error (Location_Image (Target.Location) & (+Client) & " may use " & (+Server)
                                               & ", so " & (+From) & " shall not use " & (+To));
                              end if;

                           when Forbidden_Use | Allowed_Use => null;
                              -- Those possibility have been processed before.

                        end case;
                     end if;

                  end;

               end loop;
            end if;

         end;
      end loop;
   end loop;

end Archicheck.Rules.Check;
