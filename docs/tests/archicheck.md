
# ArchiCheck code test suite



##  ArchiCheck code test suite / -lf test

  Executed:  

  > acc -lf -I src  

  Expected (34 files) :  

```  
src/archicheck.ads
src/archicheck-io.adb
src/archicheck-io.ads
...
src/archicheck-sources.ads
src/archicheck-units.adb
src/archicheck-units.ads
```  


ArchiCheck code test suite / -lf test [Successful](archicheck.md#archicheck-code-test-suite---lf-test)

##  ArchiCheck code test suite / -ld test

  > acc -ld -I ./src | sort  

  115 dependencies expected :  

```  
Archicheck.IO package body depends on Ada.Text_IO
Archicheck.IO package spec depends on Archicheck.Settings
Archicheck.Lang.Ada_Processor package body depends on Ada.Exceptions
Archicheck.Lang.Ada_Processor package body depends on Ada_Lexer
Archicheck.Lang.Ada_Processor package body depends on Ada.Text_IO
Archicheck.Lang.Ada_Processor package body depends on Archicheck.IO
Archicheck.Lang.Ada_Processor package body depends on Archicheck.Settings
Archicheck.Lang.Ada_Processor package body depends on Archicheck.Units
Archicheck.Lang.Ada_Processor package body depends on OpenToken
Archicheck.Lang.C_Processor package body depends on Ada.Exceptions
...
Archicheck.Units package body depends on Ada.Strings.Fixed
Archicheck.Units package body depends on Ada.Strings.Hash_Case_Insensitive
Archicheck.Units package body depends on Archicheck.IO
Archicheck.Units package body depends on Archicheck.Settings
Archicheck.Units package spec depends on Ada.Containers.Doubly_Linked_Lists
Archicheck.Units package spec depends on Ada.Containers.Indefinite_Hashed_Maps
Archicheck.Units package spec depends on Ada.Strings.Equal_Case_Insensitive
Archicheck.Units package spec depends on Ada.Strings.Unbounded
Archicheck.Units package spec depends on Archicheck.Sources
Archicheck.Units package spec depends on List_Image
```  


ArchiCheck code test suite / -ld test [Successful](archicheck.md#archicheck-code-test-suite---ld-test)

##  ArchiCheck code test suite / rules test

  > acc archicheck.ac -I ./src  

  Rules :  

```  
only Archicheck.Main may use Archicheck.Rules
Archicheck.Main      may use Archicheck.Lang

Archicheck.Rules may use Archicheck.Lang
Archicheck.Lang  may use Archicheck.Units
Archicheck.Units may use Archicheck.Sources

only Archicheck.Main     may use Ada.Command_Line

only Archicheck.Lang.Ada_Processor  may use Ada_Lexer 
only Archicheck.Lang.Java_Processor may use Java_Lexer

only Archicheck.Lang.Ada_Processor  may use OpenToken 
only Archicheck.Lang.Java_Processor may use OpenToken
only Archicheck.Rules.Parser        may use OpenToken

GNAT use is forbidden

Ada                 use is allowed
Archicheck.IO       use is allowed
Archicheck.Settings use is allowed
Archicheck_Config   use is allowed
List_Image          use is allowed
```  

  ![ArchiCheck dependencies view](ac_view.png)  

  No error expected  


ArchiCheck code test suite / rules test [Successful](archicheck.md#archicheck-code-test-suite--rules-test)

##  ArchiCheck code test suite / --list_non_covered

  > acc archicheck.ac -lnc -I ./src  

  Rules :  

```  
only Archicheck.Main may use Archicheck.Rules
Archicheck.Main      may use Archicheck.Lang

Archicheck.Rules may use Archicheck.Lang
Archicheck.Lang  may use Archicheck.Units
Archicheck.Units may use Archicheck.Sources

only Archicheck.Main     may use Ada.Command_Line

only Archicheck.Lang.Ada_Processor  may use Ada_Lexer 
only Archicheck.Lang.Java_Processor may use Java_Lexer

only Archicheck.Lang.Ada_Processor  may use OpenToken 
only Archicheck.Lang.Java_Processor may use OpenToken
only Archicheck.Rules.Parser        may use OpenToken

GNAT use is forbidden

Ada                 use is allowed
Archicheck.IO       use is allowed
Archicheck.Settings use is allowed
Archicheck_Config   use is allowed
List_Image          use is allowed
```  

  ![ArchiCheck dependencies view](ac_view.png)  

  No error expected  


ArchiCheck code test suite / --list_non_covered [Successful](archicheck.md#archicheck-code-test-suite----listnoncovered)
