Quick Start
===========

---

## Archicheck Overview 

---

Archicheck is a simple tool to describe and enforce architecture/design decision that are not easily checked by languages and compilers.

Archicheck needs :

- a bunch of sources : gives the directories with one or more `-I` options
- an architecture description : a simple text file describing your architecture

Let's consider for example the file `My_Architecture.txt`, that describes a simple layered architecture :

```
GUI contains pkg_1, pkg_2
DB  contains pkg_3, pkg_4

GUI is a layer over DB
```

Run ArchiCheck that way : 
`archicheck -I My_Src_Dir My_Architecture.txt` 
and it will for example checks that pkg_3 (in the lower layer) is not using pkg_2 (in the upper layer).

(More on what is checked in this case : [Layer rules test suite])

> And that's it!

---

### A real exemple : [the Batik architecture](http://svn.apache.org/repos/asf/xmlgraphics/site/deploy/batik/old/architecture.html)

![](img/Batik.png)

Here is the matching archicheck description:

```
Applications      contains Browser and Rasterizer
Core_Modules      contains UI_Component, Transcoder, SVG_Generator, Bridge and SVGDOM
Low_Level_Modules contains Renderer, GVT and SVG_Parser

Applications is a layer over Core_Modules
Core_Modules is a layer over Low_Level_Modules
```

> Easy to read, easy to write, isn't it?

## Philosophy & status quo

---


Archicheck is a mockup, with only few (but usefull) functionalities, and not tested on real, big software.
It currently process only Ada sources, but it is meant to accept other languages, and multi-languages projet. 

When I started the project, I wrote [a brief presentation of the project](Archicheck_Overview.pdf).

## Download 
A linux excutable is available here : [Download](Download.md).

---

## Building from sources

--- 

- Get ArchiCheck sources :

     Archicheck is hosted [on Github](https://github.com/LionelDraghi/ArchiCheck).
     
     (Until v0.3.0 (october 2017), archicheck was available thanks to darcs [here](https://hub.darcs.net/LioD/ArchiCheck))
     
- Get OpenToken src :
 
     download it [on GitHub](https://github.com/opentoken-io/opentoken.git)

     or do it the Debian way : 
     
     `apt install libopentoken6-dev gnat make`


- Build :

     on linux like plateform, to build the exe and run the tests :
     
     `make build`

     othewise, just build the exe : 
     
     `gnat make -Parchicheck.gpr`

     The exe should be in the Obj directory.
