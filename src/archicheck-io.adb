-- -----------------------------------------------------------------------------
-- Acc, the software architecture compliance verifier
-- Copyright (C) Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.IO body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Ada.Text_IO;

package body Archicheck.IO is

   Warnings : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Warning (Msg : in String := "") is
   begin
      Warnings := Warnings + 1;
      Put_Line ("Warning : " & Msg);
      -- use the local version of Put_Line, and not the Ada.Text_IO one,
      -- so that Warning messages are also ignored when --quiet.
   end Put_Warning;

   Errors : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Error (Msg       : in String  := "") is
   begin
      Errors := Errors + 1;
      Put_Line ("Error : " & Msg, Level => Quiet);
      -- Quiet because Error Msg should not be ignored
   end Put_Error;

   -- --------------------------------------------------------------------------
   procedure Put_Exception (Msg : in String := "") is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Warning : " & Msg);
   end Put_Exception;

   -- --------------------------------------------------------------------------
   function Error_Count   return Natural is (Errors);
   function Warning_Count return Natural is (Warnings);

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (Prefix & " | " & Msg);
      end if;
   end Put_Debug_Line;

   -- --------------------------------------------------------------------------
   --     procedure Put_Debug (Msg    : in String := "";
   --                          Debug  : in Boolean;
   --                          Prefix : in String) is
   --     begin
   --        if Debug then
   --           if Prefix = "" then
   --              Ada.Text_IO.Put (Msg);
   --           else
   --              Ada.Text_IO.Put (Prefix & " | " & Msg);
   --           end if;
   --        end if;
   --     end Put_Debug;

   -- --------------------------------------------------------------------------
   --     procedure New_Debug_Line (Debug  : in Boolean) is
   --     begin
   --        if Debug then
   --           Ada.Text_IO.New_Line;
   --        end if;
   --     end New_Debug_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item  : String;
                       Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put_Line (Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   --     procedure Put (Item  : String;
   --                    Level : Print_Out_Level := Normal) is
   --     begin
   --        if Level >= Settings.Verbosity then
   --           Ada.Text_IO.Put (Item);
   --        end if;
   --     end Put;

   -- --------------------------------------------------------------------------
   --     procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1;
   --                         Level   : Print_Out_Level := Normal) is
   --     begin
   --        if Level >= Settings.Verbosity then
   --           Ada.Text_IO.New_Line (Spacing);
   --        end if;
   --     end New_Line;

end Archicheck.IO;
