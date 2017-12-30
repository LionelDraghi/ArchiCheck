-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009, 2017 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.Sources
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Archicheck.IO;

package body Archicheck.Sources is

   -- --------------------------------------------------------------------------
   Source_List : Source_Lists.List := Source_Lists.Empty_List;

   -- --------------------------------------------------------------------------
   -- Function: Get_List
   -- --------------------------------------------------------------------------
   function Get_List return Source_Lists.List is (Source_List);

   -- --------------------------------------------------------------------------
   -- Procedure: Add_Source
   -- --------------------------------------------------------------------------
   procedure Add_Source (Src : in Source) is
   begin
      Source_List.Append (Src);
   end Add_Source;

   -- --------------------------------------------------------------------------
   -- Procedure: Dump_Sources
   -- -------------------------------------------------------------------------
   procedure Dump_Sources (Sources : in Source_Lists.List) is
      use Ada.Strings.Unbounded;
      use Archicheck.IO;
   begin
      for Src of Sources loop
         Put_Line (To_String (Src.Name), Level => Quiet);
      end loop;
   end Dump_Sources;

end Archicheck.Sources;
