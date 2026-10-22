# Command line test suite

 This test check that illegal command lines cause acc to  
 exit with a non null return code.  
 * Note that normal use is overly tested in other tests,  
   so here mainly error situations are tested.  
 * Note also that quiet and verbose mode (-q / -v) are also tested  
   in other tests.
 * Note that all diagnostics, errors as well as warnings, are put on the  
   standard output, and that the exit code is checked in each scenario :  
   1 on error, 0 on warning, and 1 on warning when -We is set.



## Scenario: Help options

  - When I successfully run `./acc -h`, 
  - Then output is
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

https://github.com/LionelDraghi/ArchiCheck
```  

## Scenario: Version option

  - When I run `./acc --version`
  - Then the output matches `0\.[0-9]+\.[0-9]+.*`

## Scenario: -I option without src dir

  - When running `./acc -I`,
  - Then I get
```  
Error : Sources directory expected after -I
```  
  - Then the exit code is `1`
  - And there is no error output

## Scenario: -I option with an unknown dir

- When running `./acc -I qsdqjh`  
- Then I get
```  
Error : No qsdqjh directory
```  
- Then the exit code is `1`
- And there is no error output

## Scenario: unknown -xyz option

- When running `./acc -xzy`  
- Then I get 
```  
Error : Unknown rules file or unknown option -xzy
```  
- Then the exit code is `1`
- And there is no error output

## Scenario: -I option with... nothing to do

(no rules file, no -ld or -lf, etc.)  

- Given the directory `dir6`    
- Given the file `dir6/src.adb`
```ada
package body Src is
end Src;
```
- When I run `./acc -I dir6`    
- Then the output contains
```
Error : Nothing to do with those sources

Acc normal use :
   acc rules_file -Ir directory [-Ir directory]*
```
- Then the exit code is `1`
- And there is no error output

## Scenario: -lr option without rules file

- When I run `./acc -lr  `
- Then the output contains
```
Error : No rules file given

Acc normal use :
   acc rules_file -Ir directory [-Ir directory]*
```
- Then the exit code is `1`
- And there is no error output


## Scenario: Legal line, but no src file in the given (existing) directory

- Given the new directory `dir9`
- When I run `./acc -lf -I dir9`  
- Then I get `Warning : Cannot list files, no sources found to analyze`
- Then the exit code is `0`
- And there is no error output

## Scenario: warnings are treated as errors when -We or --Warnings=error is set

The message is unchanged, but the exit code is now non null.

- Given the new directory `dir9`
- When I run `./acc -lf -We -I dir9` or `./acc -lf --Warnings=error -I dir9`
- Then I get `Warning : Cannot list files, no sources found to analyze`
- Then the exit code is `1`
- And there is no error output

## Scenario: file given to -I, instead of a directory

- Given new file `rules.txt` containing `Interfaces use is forbidden`
- Given file `src.adb`
```ada
procedure Src is
begin
   null;
end;
```
- When I run `./acc rules.txt -I src.adb`
- Then I get `Error : src.adb is not a directory`  
- Then the exit code is `1`
- And there is no error output

## Scenario: -ld given, but no source found

- Given new file `rules.txt` containing `Interfaces use is forbidden`
- Given new directory `dir11`
- When I run `./acc rules.txt -ld -I dir11`  
- Then I get 
```  
Warning : Cannot list dependencies, no sources found
```  
- Then the exit code is `0`
- And there is no error output

## Scenario: src found, but nothing to do with it

- Given directory `dir12`
- Given file `dir12/src.adb`
```ada
package body Src is
end;
```
- When I run `./acc -I dir12`    
- Then the output contains
```
Error : Nothing to do with those sources

Acc normal use :
   acc rules_file -Ir directory [-Ir directory]*
```
- Then the exit code is `1`
- And there is no error output


## Scenario: rules file found, but nothing to do with it

- When I run `./acc rules.txt` 
- Then the output contains
```
Error : Nothing to do with this rules file

Acc normal use :
   acc rules_file -Ir directory [-Ir directory]*
```
- Then the exit code is `1`
- And there is no error output

## Scenario: template creation (-ct and --create_template)

- Given there is no `template.ac` file
- When I run `./acc -ct` or `./acc --create_template`
- Then file `template.ac` is

```  
-- ------------------------
-- Template Acc file
-- ------------------------

-- Acc files contain :
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

-- More extensive explanations : https://github.com/LionelDraghi/ArchiCheck/blob/main/docs/rules.md
-- 
-- File generated with Acc 0.6.2-dev
```  

## Scenario: template creation when there's already one

- Given I run `./acc -ct` Successfully
- When running once more `./acc --create_template`  
- Then I get 
```  
Error : File template.ac already exists
```  
- Then the exit code is `1`
- And there is no error output

## Scenario: -ar without rule 

- When I run `./acc -ar`  or  `./acc --append_rule`  
- Then I get 
```  
Error : Rule expected after --append_rule
```  
- Then the exit code is `1`
- And there is no error output
