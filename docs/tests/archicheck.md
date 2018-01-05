
# ArchiCheck code test suite



##  ArchiCheck code test suite / -lf test

  Executed:

  > archicheck -lf -I src

  Expected (26 files) :

```
/home/lionel/Proj/Archicheck/Src/archicheck.ads
/home/lionel/Proj/Archicheck/Src/archicheck-cmd_line.adb
/home/lionel/Proj/Archicheck/Src/archicheck-cmd_line.ads
...
/home/lionel/Proj/Archicheck/Src/archicheck-sources.ads
/home/lionel/Proj/Archicheck/Src/archicheck-units.adb
/home/lionel/Proj/Archicheck/Src/archicheck-units.ads
```


ArchiCheck code test suite / -lf test [Successful](tests_status.md#successful)

##  ArchiCheck code test suite / -ld test

  > archicheck -ld -I ./src | sort

  90 dependencies expected :

```
Archicheck.Cmd_Line package body depends on Ada.Command_Line
Archicheck.Cmd_Line package body depends on Ada.Directories
Archicheck.Cmd_Line package body depends on Archicheck.IO
Archicheck.Cmd_Line package body depends on Archicheck.Lang
Archicheck.Cmd_Line package body depends on Archicheck.Settings
Archicheck.Cmd_Line package body depends on Archicheck.Sources
Archicheck.IO package body depends on Ada.Strings.Fixed
Archicheck.IO package spec depends on Ada.Text_IO
Archicheck.IO package spec depends on Archicheck.Settings
Archicheck.Lang.Ada_Processor package body depends on Ada.Exceptions
...
Archicheck.Sources package spec depends on Ada.Strings.Unbounded
Archicheck.Units package body depends on Ada.Containers.Indefinite_Hashed_Maps
Archicheck.Units package body depends on Ada.Strings.Equal_Case_Insensitive
Archicheck.Units package body depends on Ada.Strings.Fixed
Archicheck.Units package body depends on Ada.Strings.Hash_Case_Insensitive
Archicheck.Units package body depends on Archicheck.IO
Archicheck.Units package body depends on Archicheck.Settings
Archicheck.Units package spec depends on Ada.Containers.Doubly_Linked_Lists
Archicheck.Units package spec depends on Ada.Strings.Unbounded
Archicheck.Units package spec depends on Archicheck.Sources
```


ArchiCheck code test suite / -ld test [Successful](tests_status.md#successful)

##  ArchiCheck code test suite / rules test

  > archicheck archicheck.ac -I ./src

  Rules :

```
only Archicheck.Cmd_Line may use Ada.Command_Line
only Archicheck.Main     may use Ada.Command_Line

only Archicheck.Lang.Ada_Processor  may use Ada_Lexer 

only Archicheck.Lang.Java_Processor may use Java_Lexer

only Archicheck.Lang.Java_Processor may use OpenToken
only Archicheck.Rules.Parser may use OpenToken

GNAT use is forbidden
```

  ![ArchiCheck dependencies view](ac_view.png)

  No error expected


ArchiCheck code test suite / rules test [Successful](tests_status.md#successful)
