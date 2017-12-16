Quick Start
===========

---

Archicheck is a simple tool to describe and enforce architecture/design decision 
that are not easily checked by languages and compilers.

Archicheck needs :

- a bunch of sources : gives the directories with one or more `-I` options
- an architecture description : a simple text file describing your architecture

---

Let's consider for example the file `My_Architecture.txt`, that describes a simple layered architecture :

```
GUI contains pkg_1, pkg_2
DB  contains pkg_3, pkg_4

GUI is a layer over DB
```

Run ArchiCheck that way : 
`archicheck -I My_Src_Dir My_Architecture.txt` 
and it will for example checks that pkg_3 (in the lower layer) is not using pkg_2 (in the upper layer).

(More on what is checked in this case : [Layer rules test suite](#layer-rules-test-suite#)

**And that's it!**
