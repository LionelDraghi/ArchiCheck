mdtags
======

Overview
-------

`mdtags` reads text files (source code or whatever) to extrat tags provided on the command line, and extract cross-references in [markdown]() format.

For example, if you're used to disperse _FIXME_ comments across your sources, you would probably appreciate to easily build some summary.
**That's what `mdtags` is done for.**

Example
-------

Consider the following Makefile / Java / Ada code snipets:

src/Makefile:

``` Makefile
    ...
    rm -rf dir # Fixme: fail if dir doesn't exist
    ...
```

src/main.java:

``` Java
   ...
   System.out.println("Hello World!"); // Fixme: Chinese translation needed
   ...
```

src/mdtags-main.adb:

``` Ada
   ...
   raise Constraint_Error; -- Fixme:
   ...
```

`mdtags fixme -I src`
will output something like that :

```md
Source                   | Comment
------------------------ | ---------------------------
src/Makefile:13:18       | fail if dir doesn't exist
src/main.java:144:43     | Chinese translation needed
src/mdtags-main.adb:7:31 |
```

Note that `mdtags` use a GNU error message format : `sourcefile:lineno.column: message`

Output is in markdown format, a popular enhanced text file format that focus on readability.
There is a lot of tools around to deal with this format.

It will be rendered this way :

Source                   | Comment
------------------------ | ---------------------------
src/Makefile:13:18       | fail if dir doesn't exist
src/main.java:144:43     | Chinese translation needed
src/mdtags-main.adb:7:31 |

Hints:
if you're using github, just redirect `mdtags` output in a `.md` file, in your `docs` directory :  
`mdtags fixme -I src > docs/fixme.md`
