<!-- omit from toc -->
# Components and visibility matching concepts in languages

- [1. Modules](#1-modules)
- [2. Archicheck](#2-archicheck)
- [3. C](#3-c)
  - [3.1. Limitations](#31-limitations)
- [4. Ada](#4-ada)
  - [4.1. Renaming](#41-renaming)
- [5. Java](#5-java)


## 1. Modules

From a design point of view, a [Module](https://en.wikipedia.org/wiki/Modular_programming) is a small component that encapsulate code and data to perform focused on one task.

A module :

- exposes an interface
- hides internal code and data on a "need to know" basis
- may be be dependent of others modules

Note that the time as gone, and the nineties temptation of considering modules as obsoletes and classes as being the future of (among other concepts) modules has vanished.

Most languages provide mechanisms to implement those three features.
Here after, will be discussed what ArchiCheck consider as a component or a visibility relationship in each languages, and also when needed the matching of compilation units, files and modules.

## 2. Archicheck

Let's start with Archicheck :

- Modules and Components

  Archicheck make no difference between both concepts. Both are composable. If A contains B and C, Archicheck is not concerned in your decision to call A a component, B and C modules, or to call them all modules or components.
  As it was felt more general, and less linked to specific programming languages, Archicheck uses the word `component`.

- Virtual components
  
  Components appearing in the architecture or design descriptions do not always materialize through the code. Sometimes, a directory gathering source code files materialize the component name (all Gdk sources are in the Gdk subdirectory), there is a naming convention (All Gdk file name or compilation unit start with Gdk).
  Archicheck offers the possibility to explicitly declare such a component that is not defined in the code.
  It is then called a virtual component.

## 3. C

> The closest thing C has to a module is a source (.c) file. We can encapsulate functions and data in a source file to form the implementation part of a module. A corresponding header (.h) file forms the interface to the module.  
(https://www.embedded.com/modular-programming-in-c/)

Similarly, the closest thing to a visibility specification is the include preprocessor directive.

### 3.1. Limitations
- Archicheck has no [preprocessor](https://en.wikipedia.org/wiki/C_preprocessor), and do not support conditional compilation, meaning that 
  ```
  #ifdef MATRIX
  #include "matrix.h"
  #else
  #include "grid.h"
  #endif
  ```
  will report dependence to both `matrix.h` and `grid.h`, whatever is the value of `MATRIX`.


- There is no consensus on the non standard `#import directive`, that seems [inherited from Objective-C](https://stackoverflow.com/questions/39280248/what-is-the-difference-between-import-and-include-in-c).
   
This directive is not supported in Archicheck.


## 4. Ada

Archicheck build the Component hierarchy based by interpreting the names : remember that Archicheck don’t do a semantical analysis, it just does the bare minimum lexical analysis to catch components and dependencies.

### 4.1. Renaming
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

 

 

 

 

## 5. Java

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