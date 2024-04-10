
# AdaControl code test suite



##  AdaControl code test suite / -lf test

  > acc -lf -r -I adactl-1.19r10/src  

  Expected (229 files) :  

```  
adactl-1.19r10/src/a4g_bugs.adb
adactl-1.19r10/src/a4g_bugs.ads
adactl-1.19r10/src/adactl.adb
...
adactl-1.19r10/src/units_list.ads
adactl-1.19r10/src/utilities.adb
adactl-1.19r10/src/utilities.ads
```  


AdaControl code test suite / -lf test [Successful](tests_status.md#successful)

##  AdaControl code test suite / -ld test

  > acc -ld -r -I ./adactl-1.19r10/src | sort  

  1148 dependencies expected :  

```  
A4G_Bugs package body depends on Asis.Declarations
A4G_Bugs package body depends on Asis.Definitions
A4G_Bugs package body depends on Asis.Elements
A4G_Bugs package body depends on Asis.Expressions
A4G_Bugs package body depends on Thick_Queries
A4G_Bugs package body depends on Utilities
A4G_Bugs package spec depends on Asis
Adactl_Fix procedure body depends on Ada.Containers.Ordered_Maps
Adactl_Fix procedure body depends on Ada.Containers.Vectors
Adactl_Fix procedure body depends on Ada.Directories
...
Utilities package body depends on Asis.Elements
Utilities package body depends on Asis.Errors
Utilities package body depends on Asis.Implementation
Utilities package body depends on Asis.Text
Utilities package body depends on GNAT.OS_Lib
Utilities package body depends on GNAT.Traceback.Symbolic
Utilities package body depends on Thick_Queries
Utilities package spec depends on Ada.Exceptions
Utilities package spec depends on Ada.Wide_Text_IO
Utilities package spec depends on Asis
```  


AdaControl code test suite / -ld test [Successful](tests_status.md#successful)

##  AdaControl code test suite / rules test

  > acc adactl.ac -r -I ./adactl-1.19r10/src  

  Rules :  

```  
Utilities contains Scope_Manager, Thick_Queries, A4G_Bugs, String_Matching, String_Matching_Gnat, String_Matching_Portable
Utilities contains Linear_Queue, Binary_Map, Elements_Set, Adactl_Options, Implementation_Options, Options_Analyzer

Rules may use Framework

-- Exceptions :
Framework.Plugs may use Rules
Framework.Ruler may use Rules.Uncheckable

Rules     may use Utilities
Framework may use Utilities

-- Exceptions :
Adactl_Options may use Framework.Reports
Adactl_Options may use Framework.Variables.Shared_Types
```  

  No error expected  


AdaControl code test suite / rules test [Successful](tests_status.md#successful)
