Quick Start
===========

Archicheck overview
-------------------

Archicheck is a simple tool to describe and enforce simple structural aspect of the software architecture, that can't be translated at languages level.  
Or more precisely, that are translated but with a semantic loss.  

The architect think `A is a layer over B`. 

But `A` code just said `import B`, and what the software designer had in mind is just lost.  
> **The code doesn't tell the whole story**

The [Archicheck_Overview.pdf](Archicheck_Overview.pdf) provides you with some more details on that topic. 

> **Archicheck proposal is to put once for all your architecture description in simple english sentences in a text file, and to check your code compliance with that description in your test suite.**

Run it!
-------

Archicheck needs :

- a bunch of sources : give the directories with one or more `-I` options;
- an architecture description, called a [`rules file`](rules.md) : a simple text file describing your architecture.

Let's consider the following file `My_Architecture.txt`, that describes a simple layered architecture :

```
Presentation_Layer contains pkg_1, pkg_2
Application_Layer  contains pkg_3, pkg_4

Presentation_Layer is a layer over Application_Layer
```

Run ArchiCheck that way:  
> _archicheck -I src My_Architecture.txt_  

([`archicheck -h` for a complete list of options](cmd_line.md))

It will check that the code comply with your architecture.

For example, here, it will check that pkg_3 or pkg_4 (in the lower layer) are not using pkg_1 or pkg_2 (in the upper layer).


Further readings 
----------------

- [Get it and build it](building.md)
- [ArchiCheck command line](cmd_line.md)
- [More on rules and rules files](rules.md)
