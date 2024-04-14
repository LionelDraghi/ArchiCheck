
# ArchiCheck code test suite



##  ArchiCheck code test suite / -lf test

  Executed:  

  > acc -lf -I src  

  Expected (34 files) :  

```  
src/acc.ads
src/acc-io.adb
src/acc-io.ads
...
src/acc-sources.ads
src/acc-units.adb
src/acc-units.ads
```  


ArchiCheck code test suite / -lf test [Successful](archicheck.md#archicheck-code-test-suite---lf-test)

##  ArchiCheck code test suite / -ld test

  > acc -ld -I ./src | sort  

  115 dependencies expected :  

```  
Acc.IO package body depends on Ada.Text_IO
Acc.IO package spec depends on Acc.Settings
Acc.Lang.Ada_Processor package body depends on Acc.IO
Acc.Lang.Ada_Processor package body depends on Acc.Settings
Acc.Lang.Ada_Processor package body depends on Acc.Units
Acc.Lang.Ada_Processor package body depends on Ada.Exceptions
Acc.Lang.Ada_Processor package body depends on Ada_Lexer
Acc.Lang.Ada_Processor package body depends on Ada.Text_IO
Acc.Lang.Ada_Processor package body depends on OpenToken
Acc.Lang.C_Processor package body depends on Acc.IO
...
Acc.Units package body depends on Acc.IO
Acc.Units package body depends on Acc.Settings
Acc.Units package body depends on Ada.Strings.Fixed
Acc.Units package body depends on Ada.Strings.Hash_Case_Insensitive
Acc.Units package spec depends on Acc.Sources
Acc.Units package spec depends on Ada.Containers.Doubly_Linked_Lists
Acc.Units package spec depends on Ada.Containers.Indefinite_Hashed_Maps
Acc.Units package spec depends on Ada.Strings.Equal_Case_Insensitive
Acc.Units package spec depends on Ada.Strings.Unbounded
Acc.Units package spec depends on List_Image
```  


ArchiCheck code test suite / -ld test [Successful](archicheck.md#archicheck-code-test-suite---ld-test)

##  ArchiCheck code test suite / rules test

  > acc archicheck.ac -I ./src  

  Rules :  

```  
only Acc.Main may use Acc.Rules
Acc.Main      may use Acc.Lang

Acc.Rules may use Acc.Lang
Acc.Lang  may use Acc.Units
Acc.Units may use Acc.Sources

only Acc.Main may use Ada.Command_Line

only Acc.Lang.Ada_Processor  may use Ada_Lexer 
only Acc.Lang.Java_Processor may use Java_Lexer

only Acc.Lang.Ada_Processor  may use OpenToken 
only Acc.Lang.Java_Processor may use OpenToken
only Acc.Rules.Parser        may use OpenToken

GNAT use is forbidden

Ada          use is allowed
Acc.IO       use is allowed
Acc.Settings use is allowed
Acc_Config   use is allowed
List_Image   use is allowed
```  

  ![ArchiCheck dependencies view](ac_view.png)  

  No error expected  


ArchiCheck code test suite / rules test [Successful](archicheck.md#archicheck-code-test-suite--rules-test)

##  ArchiCheck code test suite / --list_non_covered

  > acc archicheck.ac -lnc -I ./src  

  Rules :  

```  
only Acc.Main may use Acc.Rules
Acc.Main      may use Acc.Lang

Acc.Rules may use Acc.Lang
Acc.Lang  may use Acc.Units
Acc.Units may use Acc.Sources

only Acc.Main may use Ada.Command_Line

only Acc.Lang.Ada_Processor  may use Ada_Lexer 
only Acc.Lang.Java_Processor may use Java_Lexer

only Acc.Lang.Ada_Processor  may use OpenToken 
only Acc.Lang.Java_Processor may use OpenToken
only Acc.Rules.Parser        may use OpenToken

GNAT use is forbidden

Ada          use is allowed
Acc.IO       use is allowed
Acc.Settings use is allowed
Acc_Config   use is allowed
List_Image   use is allowed
```  

  ![ArchiCheck dependencies view](ac_view.png)  

  No error expected  


ArchiCheck code test suite / --list_non_covered [Successful](archicheck.md#archicheck-code-test-suite----listnoncovered)
