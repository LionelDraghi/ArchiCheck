-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: Archicheck.IO specification
--
-- Purpose:
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Ada.Text_IO;

private package Archicheck.IO is

   procedure Put_Help;

   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String);
   procedure Put_Debug (Msg    : in String := "";
                        Debug  : in Boolean;
                        Prefix : in String);
   procedure New_Debug_Line (Debug  : in Boolean);

   -- Mimics eponym Text_IO functions, except that :
   --   - if --quiet is set on command line, they have no effect,
   --   - if Only_When_Verbose is False, they have no effect unless --verbose is set on command line
   procedure Put_Line (Item : String; Only_When_Verbose : Boolean := False);
   procedure Put      (Item : String; Only_When_Verbose : Boolean := False);
   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1; Only_When_Verbose : Boolean := False);

   procedure Put_Warning (Msg       : in String := "");
   procedure Put_Error   (Msg       : in String := "";
                          With_Help : in Boolean := False);

   -- -------------------------------------------------------------------------
   -- Function: GNU_Prefix
   --
   -- Purpose:
   --    This function return a source/line/column prefix to messages compatible
   --    whith GNU Standard
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --    That is :
   --       > sourcefile:lineno:
   --    if no column, or
   --       > sourcefile:lineno.column:
   --    otherwise.
   --
   --    Note that there is a trailing space, so that the message can be append
   --    directly.
   --    With the first form of the function, the displayed file name is given
   --    as an in string parameter.
   --    It is recommanded to use this form for files given on command line,
   --    to ease error msg understanding.
   --    For example, a "../../rules.txt" command line parameter should be
   --    displayed as is in case of messages regarding this rules.txt file.
   -- -------------------------------------------------------------------------
   function GNU_Prefix (File   : in String;
                        Line   : in Positive;
                        Column : in Integer := 0) return String;

   -- -------------------------------------------------------------------------
   -- Function: GNU_Prefix
   --
   -- Purpose:
   --    With this second form, the file is given as an Ada.Text_IO.File_Type,
   --    and <Ada.Directories.Full_Name> is used to display the file name.
   -- -------------------------------------------------------------------------
   function GNU_Prefix (File   : in Ada.Text_IO.File_Type;
                        Line   : in Positive;
                        Column : in Integer := 0) return String;

end Archicheck.IO;
