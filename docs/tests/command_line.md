
# Command line test suite



 This test check that illegal command lines cause archicheck to
 exit with a non null return code.
 - Note that normal use is overly tested in other tests,
   so here mainly error situations are tested.
 - Note also that quiet and verbose mode (-q / -v) are also tested
   in other tests.


##  Command line test suite / Help options:


     Test that the -h, --help or no command line will output :

  ```

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / Help options: [Successful]

##  Command line test suite / Version option


  Test that the --version will put :

  ```
0.4.2
  ```


 Command line test suite / Version option [Successful]

##  Command line test suite / -I option without src dir


  When :
  > archicheck -I
  Expecting:

  ```
Error : Sources directory expected after -I
  ```


 Command line test suite / -I option without src dir [Successful]

##  Command line test suite / -I option with an unknow dir


  When :
  > archicheck -I qsdqjh
  Expecting:

  ```
Error : No qsdqjh directory
  ```


 Command line test suite / -I option with an unknow dir [Successful]

##  Command line test suite / unknown -xyz option


  When :
  > archicheck -xzy
  Expecting:

  ```
Error : Unknown rules file or unknow option -xzy

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / unknown -xyz option [Successful]

##  Command line test suite / -I option with... nothing to do

        (no rules file, no -ld or -lf, etc.)

  When :
  > mkdir -p dir6
  > touch dir6/src.adb
  > archicheck -I dir6
  Expecting:

  ```
Error : Nothing to do with those sources

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / -I option with... nothing to do [Successful]

##  Command line test suite / -lr option without rules file


  When :
  > archicheck -lr
  Expecting:

  ```
Error : No rules file given

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / -lr option without rules file [Successful]

##  Command line test suite / Legal line, but no src file in the given (existing) directory


  When :
  > mkdir dir9
  > archicheck -lf -I dir9
  Expecting:

  ```
Error : Cannot list files, no sources found to analyze
  ```


 Command line test suite / Legal line, but no src file in the given (existing) directory [Successful]

##  Command line test suite / file given to -I, instead of a directory


  When :
  > touch rules.txt src.adb
  > archicheck rules.txt -I src.adb
  Expecting:

  ```
Error : src.adb is not a directory
  ```


 Command line test suite / file given to -I, instead of a directory [Successful]

##  Command line test suite / -ld given, but no source found


  When :
  > mkdir -p dir11
  > touch rules.txt
  > archicheck rules.txt -ld -I dir11
  Expecting:

  ```
Error : Cannot list dependencies, no sources found

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / -ld given, but no source found [Successful]

##  Command line test suite / src found, but nothing to do whith it


  When :
  > mkdir -p dir12
  > touch dir12/src.adb
  > archicheck -I dir12
  Expecting:

  ```
Error : Nothing to do with those sources

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / src found, but nothing to do whith it [Successful]

##  Command line test suite / rules file found, but nothing to do whith it


  When :
  > archicheck rules.txt
  Expecting:

  ```
Error : Nothing to do with this rules file

ArchiCheck normal use :
   archicheck rules_file -I directory [-I directory]*

General form :
   archicheck [Options] [rules_file] [-I directory]*

Options :
   -lf | --list_files        : list sources files analyzed
   -ld | --list_dependencies : list identified units and dependencies in analyzed sources files
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


 Command line test suite / rules file found, but nothing to do whith it [Successful]
