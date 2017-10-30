-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Components body
-- Purpose:
--    Define a Component, the Unit_List each Component contains, and the global
--    Component Map.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------


package body Archicheck.Components is

   -- --------------------------------------------------------------------------
   -- Function: Unit_List_Image
   -- --------------------------------------------------------------------------
   function Unit_List_Image (UL : Unit_Lists.List) return String is
      First_Unit : Boolean := True;
      use Ada.Strings.Unbounded;
      Tmp : Unbounded_String := Null_Unbounded_String;
   begin
      for U of UL loop
         if First_Unit then
            Tmp := U;
            First_Unit := False;
         else
            Tmp := Tmp & " and " & U;
         end if;
      end loop;
      return (To_String (Tmp));
   end Unit_List_Image;

   -- --------------------------------------------------------------------------
   -- Function: Component_List_Image
   -- --------------------------------------------------------------------------
   function Component_List_Image return String is
      use Ada.Strings.Unbounded;
      -- use IO;
      Tmp : Unbounded_String := Null_Unbounded_String;
   begin
      for C in Component_Map.Iterate loop
         Tmp := To_Unbounded_String (Component_Maps.Key (C)) & " contains " & Unit_List_Image (Component_Map (C)); --- & ASCII.LF;
      end loop;
      return (To_String (Tmp));
   end Component_List_Image;

   --     -- -------------------------------------------------------------------------
   --     function Put_Unit_List (UL : Unit_Lists.List) is
   --        First_Unit : Boolean := True;
   --        use Ada.Strings.Unbounded;
   --     begin
   --        for U of UL loop
   --           if First_Unit then
   --              Put (To_String (U));
   --              First_Unit := False;
   --           else
   --              Put (" and " & To_String (U));
   --           end if;
   --        end loop;
   --        New_Line;
   --     end Put_Unit_List;


end Archicheck.Components;
