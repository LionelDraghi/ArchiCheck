-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Layers body
--
-- Purpose:
--    This package defines a dependency between two layers, and manage
--    the Layers_List.
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

package body Archicheck.Layers is

   -- --------------------------------------------------------------------------
   Layers : Layer_Relationship_Lists.List;

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Layer_Relationship_Lists.List is (Layers);

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Layer
   -- --------------------------------------------------------------------------
   procedure Add_Layer (Layer : in Layer_Relationship) is
   begin
      Layers.Append (Layer);
   end Add_Layer;

end Archicheck.Layers;
