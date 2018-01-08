Archicheck command line
=======================

Archicheck command line
-----------------------

```
archicheck -h
```

```

ArchiCheck normal use :
   archicheck rules_file -r -I directory [-I directory]*

General form :
   archicheck [Queries] [rules_file] [Options]* [-I directory]*

Options :
   -r  | --recursive      : all following -I are recursive
   -We | --Warnings=error : Treat warnings as errors
   -v  | --verbose
   -q  | --quiet          : no message unless error. Warning are also ignored.
         --version        : archicheck version
   -h  | --help           : this message

Queries :
   -lf | --list_files        : list analyzed sources files
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
   -lr | --list_rules        : list rules in a rules file
   If any, only one of the queries is performed,
   and then the full analisys on sources is not done.

Examples:
   archicheck rules.txt -I ./src
   archicheck -lf -I ./src
   archicheck -lr rules.txt

http://lionel.draghi.free.fr/Archicheck/index.html

```

Archicheck current version
--------------------------

```
archicheck --version
```

```
0.5.5

```

