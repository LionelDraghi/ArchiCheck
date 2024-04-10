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
   Put_Line ("   acc rules_file -Ir directory [-Ir directory]*");
   New_Line;
   Put_Line ("General form :");
   Put_Line ("   acc [Options]* [Queries] [-ar | --append_rule 'some rule']* [rules_file] [-I[r] directory]*");
   New_Line;
   Put_Line ("   -I  src : looks for sources in src dir");
   Put_Line ("   -Ir src : looks for sources in src dir and subdirs");
   New_Line;
   Put_Line ("Rules :");
   Put_Line ("   Rules may be in a text file, or directly in the command line prefixed with -ar");
   Put_Line ("   When both are provided, rules in command line are appended to rules in the file");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -r  | --recursive      : all following -I are recursive");
   Put_Line ("   -We | --Warnings=error : treat warnings as errors");
   Put_Line ("   -v  | --verbose");
   Put_Line ("   -q  | --quiet          : no message unless error. Warning are also ignored.");
   Put_Line ("         --version        : acc version");
   Put_Line ("   -h  | --help           : this message");
   New_Line;
   Put_Line ("Queries :");
   Put_Line ("   -lf  | --list_files        : list analyzed sources files");
   Put_Line ("   -ld  | --list_dependencies : list identified units and dependencies"
             & " in analyzed sources files");
   Put_Line ("   -lr  | --list_rules        : list rules in a rules file");
   Put_Line ("   -lnc | --list_non_covered  : list compilation units not involved in rules file");
   Put_Line ("   -ct  | --create_template   : create a commented example of rules file");
   Put_Line ("   If any, only one of the queries is performed");
   Put_Line ("   and the full analysis on sources is not done.");
   New_Line;
   Put_Line ("Use examples:");
   Put_Line ("   acc rules.txt -Ir src");
   Put_Line ("   acc -lf -Ir src");
   Put_Line ("   acc -lr rules.txt");
   Put_Line ("   acc -ar 'Java.IO use is forbidden' -Ir src");
   New_Line;
   Put_Line ("Rules file:");
   Put_Line ("   To start a new rules file, run:");
   Put_Line ("   acc -ct");
   Put_Line ("   A commented template.ac file will be created : rename it and edit it.");
   New_Line;
   Put_Line ("http://lionel.draghi.free.fr/Archicheck/index.html");
   New_Line;
end Put_Help;
