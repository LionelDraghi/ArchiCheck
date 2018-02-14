
# Command line test suite



 This test check that illegal command lines cause archicheck to
 exit with a non null return code.
 - Note that normal use is overly tested in other tests,
   so here mainly error situations are tested.
 - Note also that quiet and verbose mode (-q / -v) are also tested
   in other tests.


##  Command line test suite / Help options


  Test that the -h, --help or no command line will output :

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
   and the full analisys on sources is not done.

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


Command line test suite / Help options [Successful](tests_status.md#successful)

##  Command line test suite / Version option


  Test that the --version will put :

```
0.5.7
```


Command line test suite / Version option [Successful](tests_status.md#successful)

##  Command line test suite / -I option without src dir


  When running:
  > archicheck -I

  Expecting:

```
Error : Sources directory expected after -I
```


Command line test suite / -I option without src dir [Successful](tests_status.md#successful)

##  Command line test suite / -I option with an unknow dir


  When running:
  > archicheck -I qsdqjh

  Expecting:

```
Error : No qsdqjh directory
```


Command line test suite / -I option with an unknow dir [Successful](tests_status.md#successful)

##  Command line test suite / unknown -xyz option


  When running:
  > archicheck -xzy

  Expecting:

```
Error : Unknown rules file or unknow option -xzy

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
   and the full analisys on sources is not done.

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


Command line test suite / unknown -xyz option [Successful](tests_status.md#successful)

##  Command line test suite / -I option with... nothing to do

  (no rules file, no -ld or -lf, etc.)

  When running:
  > mkdir -p dir6  
  > touch dir6/src.adb  
  > archicheck -I dir6  

  Expecting:

```
Error : Nothing to do with those sources

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
   and the full analisys on sources is not done.

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


Command line test suite / -I option with... nothing to do [Successful](tests_status.md#successful)

##  Command line test suite / -lr option without rules file


  When running:
  > archicheck -lr

  Expecting:

```
Error : No rules file given

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
   and the full analisys on sources is not done.

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


Command line test suite / -lr option without rules file [Successful](tests_status.md#successful)

##  Command line test suite / Legal line, but no src file in the given (existing) directory


  When running:
  > mkdir dir9  
  > archicheck -lf -I dir9  

  Expecting:

```
Warning : Cannot list files, no sources found to analyze
```

  archicheck return 1 if -We or --Warnings=error

Command line test suite / Legal line, but no src file in the given (existing) directory [Successful](tests_status.md#successful)

##  Command line test suite / file given to -I, instead of a directory


  When running:
  > touch rules.txt src.adb  
  > archicheck rules.txt -I src.adb  

  Expecting:

```
Error : src.adb is not a directory
```


Command line test suite / file given to -I, instead of a directory [Successful](tests_status.md#successful)

##  Command line test suite / -ld given, but no source found


  When running:
  > mkdir -p dir11

  > echo Interfaces use is forbidden > rules.txt

  > archicheck rules.txt -ld -I dir11

  Expecting:

```
Warning : Cannot list dependencies, no sources found
```


Command line test suite / -ld given, but no source found [Successful](tests_status.md#successful)

##  Command line test suite / src found, but nothing to do whith it


  When running:
  > mkdir -p dir12  
  > touch dir12/src.adb  
  > archicheck -I dir12  

  Expecting:

```
Error : Nothing to do with those sources

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
   and the full analisys on sources is not done.

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


Command line test suite / src found, but nothing to do whith it [Successful](tests_status.md#successful)

##  Command line test suite / rules file found, but nothing to do whith it


  When running:
  > archicheck rules.txt

  Expecting:

```
Error : Nothing to do with this rules file

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
   and the full analisys on sources is not done.

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


Command line test suite / rules file found, but nothing to do whith it [Successful](tests_status.md#successful)

##  Command line test suite / template creation (-ct and --create_template)


  When running:
  > archicheck -ct

  Expecting file template.ac:

```
-- ------------------------
-- Template ArchiCheck file
-- ------------------------

-- ArchiCheck files contain :
-- 1. Comments, prefixed by '--', '\\' or '#'

-- 2. Component definitions, like: 
Application_Layer contains pkg_1, pkg_2, pkg_3
--    Application_Layer is the component name.
--    It contains compilation units (pkg_1, etc.), or other
--    components, meaning that you can define nested components.

-- 3. Rules on units and components
Layer_A is a layer over Layer_B            -- Layer declaration
Pango may use Cairo                        -- Use declaration
Only Layer_B may use Interfaces.C          -- Restricted use declaration
Ada.Containers.Indefinite use is forbidden -- Forbidden use
Java.IO use is allowed                     -- Allowed use

--    Note that wildcard are not yet implemented, but
--    Java.IO means Java.IO and Java.IO.*

-- More exensive explanations : http://lionel.draghi.free.fr/Archicheck/rules/
-- 
-- File generated with ArchiCheck 0.5.7
```

  When running:
  > archicheck --create_template
  Expecting error:
```
Error : File template.ac already exists
```


Command line test suite / template creation (-ct and --create_template) [Successful](tests_status.md#successful)
