<!-- omit from toc -->
# Components and visibility matching concepts in languages

Languages by alphabetical order, with a limitations list at the end.

## 1. Ada

Archicheck build the Component hierarchy based by interpreting the names : remember that Archicheck don’t do a semantical analysis, it just does the bare minimum lexical analysis to catch components and dependencies.

### Renaming
In Ada, there is a possibility to rename a package miming a child package.
For example :

```
package OS.Disk_Driver renames Disk_Driver;
```

But this does not transform Disk_Driver in a real child package : it won’t gain the visibility on OS that a real child has, and it cannot be extended with a new child :

```
package OS.Disk_Driver.SSD is
```

Is illegal.

ArchiCheck ignore that Ada rule, will store `Disk_Driver` as a child of OS, `SSD` as `Disk_Driver` child.
Telling that this is illegal is the compiler's job.

For the same reason, ArchiCheck, take into account private packages as normal packages.

Considering :

```
package API is …

private package API.Utilities is…
```
 
One could write a rule :

```
X may use API.Utilities
```

It’s not possible, but Archicheck is not going to warn the user. Once again, this is the compiler responsibility.
The good news is that if X try to use API.Utilities, it won’t compile.

Similarly, limited with and private with are considered as normal with :
for Archicheck : `limited with P;` = `limited private with P;` = `private with P;` = `with P;`

I did not notice any pathological use of Ada package that breaks ArchiCheck behavior till now.

 
## 2. C

> The closest thing C has to a module is a source (.c) file. We can encapsulate functions and data in a source file to form the implementation part of a module. A corresponding header (.h) file forms the interface to the module.  
(https://www.embedded.com/modular-programming-in-c/)

It's a convention to have interface in the `.h` file and implementation in the `.c` file.
Both `x.h` and `x.c`, or just one of those files, constitute the `x` Compilation Unit for `Acc`. 

The closest thing to a visibility specification is the `include` preprocessor directive.

> :Warning: 
Note that `Acc` does not take into account [preprocessing](https://en.wikipedia.org/wiki/C_preprocessor) or macro expansion, that is the fact that what is compiled may be quite different from the source files. 

In more precise words, according [to Wikipedia]https://en.wikipedia.org/wiki/Translation_unit_(programming) :

> A C program consists of units called source files (or preprocessing files), which, in addition to source code, includes directives for the C preprocessor. A translation unit is the output of the C preprocessor – a source file after it has been preprocessed.

> Preprocessing notably consists of expanding a source file to recursively replace all #include directives with the literal file declared in the directive (usually header files, but possibly other source files); the result of this step is a preprocessing translation unit. Further steps include macro expansion of #define directives, and conditional compilation of #ifdef directives, among others; this translates the preprocessing translation unit into a translation unit. From a translation unit, the compiler generates an object file, which can be further processed and linked (possibly with other object files) to form an executable program. 

`Acc Compilation Units` do not directly matches `C translation units`, but `preprocessing files`.

The only consequence I am aware of, is on conditional includes. In the case of :
```C
#ifdef LINUX
#include "linux_if.h"
#else
#include "windows_if.h"
#endif
```

`Acc` wil consider both as dependencies.
This not true for both the Windows and the Linux exe, but this true from a sources point of view.
I see no need to change that at that point.

 ## 3. C++

> [!NOTE] : C++ processing Not Yet Implemented

C++ introduce some concepts over C, with the typical C++ complexity.
Compilation units are generally called [Translation units](https://en.wikipedia.org/wiki/Translation_unit_(programming)).

## 4. Java

[Here is a tutorial on Java packages](https://docs.oracle.com/javase/tutorial/java/package/packages.html)


 According to Java standards and common Java practices,
every class stands in its own source file.
 However, it is possible to have multiple classes in a single
 file, provided that there is only one public.
 All the top-level non-public types will be package private.

 Processing private classes is by definition of no interest
 for ArchiCheck, so there is no problem in exiting here.

 On the other hand, it is possible to import the public
 nested classes of an enclosing class.
 sure what can be the consequences of processing such an
, but not processing nested classes in this parser.
 anyway, as it would be costly in time processing, and
 raise far more complex the lexer, I don't intend to
 change this code for now.

## 5. Limitations Overview

### C

- `import` directive
  
  There is no consensus on the non standard `#import` directive, that seems [inherited from Objective-C](https://stackoverflow.com/questions/39280248/what-is-the-difference-between-import-and-include-in-c).
  This directive is **not** supported in Archicheck.

### Java

- detection of visibility through Java fully qualified name ("import"
  is not mandatory in Java to use classes) is **NOT** done at this stage.


