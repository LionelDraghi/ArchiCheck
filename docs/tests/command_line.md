
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


Command line test suite / Help options [Successful](command_line.md#command-line-test-suite--help-options)

##  Command line test suite / Version option


  Test that the --version will put :  

```  
0.5.8
```  


Command line test suite / Version option [Successful](command_line.md#command-line-test-suite--version-option)

##  Command line test suite / -I option without src dir


  When running:  
  > acc -I  

  Expecting:  

```  
Error : Sources directory expected after -I
```  


Command line test suite / -I option without src dir [Successful](command_line.md#command-line-test-suite---i-option-without-src-dir)

##  Command line test suite / -I option with an unknown dir


  When running:  
  > acc -I qsdqjh  

  Expecting:  

```  
Error : No qsdqjh directory
```  


Command line test suite / -I option with an unknown dir [Successful](command_line.md#command-line-test-suite---i-option-with-an-unknown-dir)

##  Command line test suite / unknown -xyz option


  When running:  
  > acc -xzy  

  Expecting:  

```  
Error : Unknown rules file or unknown option -xzy
```  


Command line test suite / unknown -xyz option [Successful](command_line.md#command-line-test-suite--unknown--xyz-option)

##  Command line test suite / -I option with... nothing to do

  (no rules file, no -ld or -lf, etc.)  

  When running:  
  > mkdir -p dir6    
  > touch dir6/src.adb    
  > acc -I dir6    

  Expecting:  

```  
Error : Nothing to do with those sources

ArchiCheck normal use :
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


Command line test suite / -I option with... nothing to do [Successful](command_line.md#command-line-test-suite---i-option-with-nothing-to-do)

##  Command line test suite / -lr option without rules file


  When running:  
  > acc -lr  

  Expecting:  

```  
Error : No rules file given

ArchiCheck normal use :
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


Command line test suite / -lr option without rules file [Successful](command_line.md#command-line-test-suite---lr-option-without-rules-file)

##  Command line test suite / Legal line, but no src file in the given (existing) directory


  When running:  
  > mkdir dir9    
  > acc -lf -I dir9    

  Expecting:  

```  
Warning : Cannot list files, no sources found to analyze
```  

  archicheck return 1 if -We or --Warnings=error  

Command line test suite / Legal line, but no src file in the given (existing) directory [Successful](command_line.md#command-line-test-suite--legal-line-but-no-src-file-in-the-given-existing-directory)

##  Command line test suite / file given to -I, instead of a directory


  When running:  
  > touch rules.txt src.adb    
  > acc rules.txt -I src.adb    

  Expecting:  

```  
Error : src.adb is not a directory
```  


Command line test suite / file given to -I, instead of a directory [Successful](command_line.md#command-line-test-suite--file-given-to--i-instead-of-a-directory)

##  Command line test suite / -ld given, but no source found


  When running:  
  > mkdir -p dir11  

  > echo Interfaces use is forbidden > rules.txt  

  > acc rules.txt -ld -I dir11  

  Expecting:  

```  
Warning : Cannot list dependencies, no sources found
```  


Command line test suite / -ld given, but no source found [Successful](command_line.md#command-line-test-suite---ld-given-but-no-source-found)

##  Command line test suite / src found, but nothing to do whith it


  When running:  
  > mkdir -p dir12    
  > touch dir12/src.adb    
  > acc -I dir12    

  Expecting:  

```  
Error : Nothing to do with those sources

ArchiCheck normal use :
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


Command line test suite / src found, but nothing to do whith it [Successful](command_line.md#command-line-test-suite--src-found-but-nothing-to-do-whith-it)

##  Command line test suite / rules file found, but nothing to do whith it


  When running:  
  > acc rules.txt  

  Expecting:  

```  
Error : Nothing to do with this rules file

ArchiCheck normal use :
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


Command line test suite / rules file found, but nothing to do whith it [Successful](command_line.md#command-line-test-suite--rules-file-found-but-nothing-to-do-whith-it)

##  Command line test suite / template creation (-ct and --create_template)


  When running:  
  > acc -ct  

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

-- More extensive explanations : http://lionel.draghi.free.fr/Archicheck/rules/
-- 
-- File generated with ArchiCheck 0.5.8
```  

  When running once more :  
  > acc --create_template  
  Expecting error:  
```  
Error : File template.ac already exists
```  


Command line test suite / template creation (-ct and --create_template) [Successful](command_line.md#command-line-test-suite--template-creation--ct-and---createtemplate)

##  Command line test suite / -ar without rule 


  When running:  
  > acc -ar  
  or  
  > acc --apend_rule  

  Expecting:  
```  
Error : Rule expected after --append_rule
```  


Command line test suite / -ar without rule [Successful](command_line.md#command-line-test-suite---ar-without-rule)
