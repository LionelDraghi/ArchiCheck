Quick Start
===========

Archicheck overview
-------------------

Archicheck is a simple tool to describe and enforce simple structural aspect of the software architecture, that can't be translated at languages level.  
Or more precisely, that are translated but with a semantic loss.  

The architect think `A is a layer over B`. 

But `A` code just said `import B`, and what the software designer had in mind is just lost.  

The [Archicheck_Overview.pdf](Archicheck_Overview.pdf) provides you with some more details on that topic. 


Run it!
-------

Archicheck needs :

- a bunch of sources : give the directories with one or more `-I` options;
- an architecture description, called a [`rules file`](rules.md) : a simple text file describing your architecture.

Let's consider the following file `My_Architecture.txt`, that describes a simple layered architecture :

```
GUI contains pkg_1, pkg_2
DB  contains pkg_3, pkg_4

GUI is a layer over DB
```

Run ArchiCheck that way :  
`archicheck -I My_Src_Dir My_Architecture.txt`  

It will check that the code comply with your architecture.

For example, here, it will check that pkg_3 or pkg_4 (in the lower layer) are not using pkg_1 or pkg_2 (in the upper layer).


Further readings 
----------------

- [Get it and build it](building.md)
- [ArchiCheck command line](cmd_line.md)
- [More on rules and rules files](rules.md)
