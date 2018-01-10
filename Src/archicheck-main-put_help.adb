-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005 to 2018 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (Archicheck.Main)

-- --------------------------------------------------------------------------
procedure Put_Help is
begin
   New_Line;
   Put_Line ("ArchiCheck normal use :");
   Put_Line ("   archicheck rules_file -r -I directory [-I directory]*");
   New_Line;
   Put_Line ("General form :");
   Put_Line ("   archicheck [Queries] [rules_file] [Options]* [-I directory]*");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -r  | --recursive      : all following -I are recursive");
   Put_Line ("   -We | --Warnings=error : Treat warnings as errors");
   Put_Line ("   -v  | --verbose");
   Put_Line ("   -q  | --quiet          : no message unless error. Warning are also ignored.");
   Put_Line ("         --version        : archicheck version");
   Put_Line ("   -h  | --help           : this message");
   New_Line;
   Put_Line ("Queries :");
   Put_Line ("   -lf  | --list_files        : list analyzed sources files");
   Put_Line ("   -ld  | --list_dependencies : list identified units and dependencies"
             & " in analyzed sources files");
   Put_Line ("   -lr  | --list_rules        : list rules in a rules file");
   Put_Line ("   -lnc | --list_non_covered  : list units not involved in rules file");
   Put_Line ("   If any, only one of the queries is performed,");
   Put_Line ("   and then the full analisys on sources is not done.");
   New_Line;
   Put_Line ("Examples:");
   Put_Line ("   archicheck rules.txt -I ./src");
   Put_Line ("   archicheck -lf -I ./src");
   Put_Line ("   archicheck -lr rules.txt");
   New_Line;
   Put_Line ("http://lionel.draghi.free.fr/Archicheck/index.html");
   New_Line;
end Put_Help;
