-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Rules specification
--
-- Purpose:
--    This package defines the three different possible rules in the rules
--    file, that is :
--    - the list of units wich use is always forbidden
--    - the list of units wich use is always allowed
--    - the list of possible relationship between two units
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Archicheck.Sources;
with Archicheck.Units; use Archicheck.Units;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

private package Archicheck.Rules is

private
   -- --------------------------------------------------------------------------
   type Rule_Kind is (Layer_Over,    -- X is a layer over Y
                      May_Use,       -- X may use Y
                      Exclusive_Use, -- only X may use Y
                      Forbidden_Use, -- X use is forbidden
                      Allowed_Use);  -- Y use is allowed
   subtype With_Object_Rule_Kind is
     Rule_Kind range Rule_Kind'First .. Exclusive_Use;
   subtype No_Object_Rule_Kind is
     Rule_Kind range Forbidden_Use .. Rule_Kind'Last;
   -- Some rules have a subject X and an object Y :
   --   X may use Y
   -- Some rules have only a subject X :
   --   X use is forbidden

   type Rule (Kind : Rule_Kind) is record
      Location     : Sources.Location; -- where the rule comes from
      Subject_Unit : Unit_Name;
      case Kind is
         when With_Object_Rule_Kind => Object_Unit : Unit_Name;
         when No_Object_Rule_Kind   => null;
      end case;
   end record;

   procedure Add_Rule (R : in Rule);

   -- --------------------------------------------------------------------------
   package Rule_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Rule);

   function Get_With_Object_Rule_List return Rule_Lists.List;

   -- --------------------------------------------------------------------------
   function Is_Forbidden (Unit : in Unit_Name) return Boolean;
   function Is_Allowed   (Unit : in Unit_Name) return Boolean;
   -- Those functions check only rules without object.

   -- --------------------------------------------------------------------------
   function Is_Allowed (Unit     : in Unit_Name;
                        For_Unit : in Unit_Name) return Boolean;
   -- This function checks only rules with object.

   -- --------------------------------------------------------------------------
   function Allowed_Users (Of_Unit : in Unit_Name) return Rule_Lists.List;
   -- returns the rules with object that have Of_Unit as target
   function Users_Image (List : in Rule_Lists.List) return String;
   -- returns "X and Y and Z" or "X" or ""

   -- --------------------------------------------------------------------------
   function Is_Involved_In_A_Rule (Unit : in Unit_Name) return Boolean;

end Archicheck.Rules;
