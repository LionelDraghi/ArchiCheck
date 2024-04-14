-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Acc.Rules specification
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

with Acc.Sources;
with Acc.Units; use Acc.Units;

with List_Image;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

private package Acc.Rules is

private
   -- --------------------------------------------------------------------------
   -- Some rules have a subject X and an object Y :
   --   X may use Y
   -- Some rules have only a subject X :
   --   X use is forbidden
   type Rule_Kind is (-- without object
                      Allowed_Use,     -- Y use is allowed
                      Forbidden_Use,   -- X use is forbidden
                      -- with object
                      Exclusive_Use,   -- only X may use Y
                      Are_Independent, -- X and Y are independent
                      Layer_Over,      -- X is a layer over Y
                      May_Use          -- X may use Y
                      );
   subtype No_Object_Rule_Kind is
     Rule_Kind range Rule_Kind'First .. Forbidden_Use;
   subtype With_Object_Rule_Kind is
     Rule_Kind range Exclusive_Use .. Rule_Kind'Last;

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

   function Get_With_Object_Rule_List  return Rule_Lists.List;
   function Get_Subject_Only_Rule_List return Rule_Lists.List;

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

   -- --------------------------------------------------------------------------
   use Rule_Lists;
   package Rule_Lists_Cursors is new List_Image.Cursors_Signature
     (Container => List,
      Cursor    => Cursor);
   function Image (C : Cursor) return String is
     (+(Element (C).Subject_Unit));
   function Users_Image is new List_Image.Image
   -- returns "X, Y and Z" or "X and Y" or "X" or ""
     (Cursors => Rule_Lists_Cursors,
      Style   => List_Image.English_Style);

   -- --------------------------------------------------------------------------
   function Is_Involved_In_A_Rule (Unit : in Unit_Name) return Boolean;

end Acc.Rules;
