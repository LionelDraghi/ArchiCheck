Archicheck command line
=======================

Archicheck command line
-----------------------

```
archicheck -h
```

```

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified dependencies in analyzed sources files
   -lr | --list_rules        : list rules in a rules file
   -r  | --recursive         : all following -I are recursive
   -v  | --verbose
   -q  | --quiet             : no message unless error. Warning are also ignored.
         --version           : archicheck version
   -h  | --help              : this message

Examples:
   archicheck rules.txt -I ./src
   archicheck -lf -I ./src
   archicheck -lr rules.txt

```

Archicheck current version
--------------------------

```
archicheck --version
```

```
0.4.0

```

