Archicheck command line
=======================

Archicheck command line
-----------------------

```
archicheck -h
```

```

ArchiCheck normal use :
   archicheck rules_file -Ir directory [-Ir directory]*

General form :
   archicheck [Queries] [rules_file] [Options]* [-I[r] directory]*

   -I  src : looks for sources in src dir
   -Ir src : looks for sources in src dir and subdirs

Options :
   -r  | --recursive      : all following -I are recursive
   -We | --Warnings=error : treat warnings as errors
   -v  | --verbose
   -q  | --quiet          : no message unless error. Warning are also ignored.
         --version        : archicheck version
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
   archicheck rules.txt -Ir src
   archicheck -lf -Ir src
   archicheck -lr rules.txt

Rules file:
   To start a new rules file, run:
   archicheck -ct
   A commented template.ac file will be created : rename it and edit it.

http://lionel.draghi.free.fr/Archicheck/index.html

```

Archicheck current version
--------------------------

```
archicheck --version
```

```
0.5.8
```

