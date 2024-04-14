Archicheck command line
=======================

Archicheck command line
-----------------------

```
acc -h
```

```

Acc normal use :
   acc rules_file -Ir directory [-Ir directory]*

General form :
   acc [Options]* [Queries] [-ar | --append_rule 'some rule']* [rules_file] [-I[r] directory]*

   -I  src : looks for sources in src dir
   -Ir src : looks for sources in src dir and subdirs

Rules :
   Rules may be in a text file, or directly in the command line prefixed with -ar
   When both are provided, rules in command line are appended to rules in the file

Options :
   -r  | --recursive      : all following -I are recursive
   -We | --Warnings=error : treat warnings as errors
   -v  | --verbose
   -q  | --quiet          : no message unless error. Warning are also ignored.
         --version        : acc version
   -h  | --help           : this message

Queries :
   -lf  | --list_files        : list analyzed sources files
   -ld  | --list_dependencies : list identified units and dependencies in analyzed sources files
   -lr  | --list_rules        : list rules in a rules file
   -lnc | --list_non_covered  : list compilation units not involved in rules file
   -ct  | --create_template   : create a commented example of rules file
   If any, only one of the queries is performed
   and the full analysis on sources is not done.

Use examples:
   acc rules.txt -Ir src
   acc -lf -Ir src
   acc -lr rules.txt
   acc -ar 'Java.IO use is forbidden' -Ir src

Rules file:
   To start a new rules file, run:
   acc -ct
   A commented template.ac file will be created : rename it and edit it.

http://lionel.draghi.free.fr/Archicheck/index.html

```

Archicheck current version
--------------------------

```
acc --version
```

```
0.6.0
```

