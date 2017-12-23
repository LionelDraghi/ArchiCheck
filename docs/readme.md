Quick Start
===========

Archicheck overview
-------------------
---
Archicheck is a simple tool to describe and enforce architecture/design decisions 
that can't be translated at languages level.  
Or more precisely, that are translated but with a semantic loss.  

The architect think `A is a layer over B`, but `A` code just said `import B`, and that's not a small loss.  
The (old) [Archicheck_Overview.pdf](Archicheck_Overview.pdf) provides you with some more details on that topic. 

Run it!
-------
---

Archicheck needs :

- a bunch of sources : gives the directories with one or more `-I` options
- an architecture description : a simple text file describing your architecture

Let's consider the file `My_Architecture.txt`, that describes a simple layered architecture :

```
GUI contains pkg_1, pkg_2
DB  contains pkg_3, pkg_4

GUI is a layer over DB
```

Run ArchiCheck that way :  
`archicheck -I My_Src_Dir My_Architecture.txt`  
and it will for example checks that pkg_3 (in the lower layer) is not using pkg_2 (in the upper layer).

> And that's it!


Further readings 
----------------
---
- [Get it and build it](#contributing.md)
- [ArchiCheck command line](#cmd-line.md)
- [More on rules files](#rules.md)


