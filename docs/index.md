<!-- omit from toc -->
[ArchiCheck](http://lionel.draghi.free.fr/Archicheck/index.html)
================================================================

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) ![](generated_img/version.svg) ![](generated_img/tests_ok.svg) ![](generated_img/tests_ko.svg) [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/1625/badge)](https://bestpractices.coreinfrastructure.org/projects/1625) [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/archicheck.json)](https://alire.ada.dev/crates/archicheck.html)

Contents:
- [Overview](#overview)
- [Get it!](#get-it)
- [Run it!](#run-it)
- [Helpful hints](#helpful-hints)
- [Further reading](#further-reading)
- [Help and comments](#help-and-comments)

---------------------------------------------------------------------

## Overview

Simple structural aspect of the software architecture, metaphor like *My software is a layered system*, can not be fully translated at programming languages level. Even in languages like Ada, powerful regarding description of the software structure, there is a semantic loss.

![The code doesn't tell the whole story!](semantic_gap.png)

Sooner or later, in large or complex development, someone will propose a patch adding an `import` that compile fine, but is a complete violation of the architecture.

![Architecture degradation over time](architecture_degradation.png)

Archicheck is a simple [free software](copying.md) tool that :

1. ease simple architecture description.  
   > It's really as simple as: _Gtk is a layer over Gdk_.
2. enforce code compliance with that description.  
   > Put acc in your test suite, and let it be a vigilant teacher of your architecture over time.

---------------------------------------------------------------------

## Get it!

[Source or exe download](building.md)

---------------------------------------------------------------------

## Run it!

Archicheck needs :

- a bunch of sources : give the directories with one or more `-I` options;
- an architecture description, called a [`rules file`](rules.md) : a simple text file describing your architecture.

Let's consider the following `My_Architecture.ac` file, that describes a simple layered architecture :

```
Presentation_Layer contains pkg_1, pkg_2
Application_Layer  contains pkg_3, pkg_4

Presentation_Layer is a layer over Application_Layer
```

Run ArchiCheck that way:  
`acc -I src My_Architecture.txt`  

It will check that the code comply with your architecture.

For example, here, it will check that pkg_3 or pkg_4 (in the lower layer) are not using pkg_1 or pkg_2 (in the upper layer).

---------------------------------------------------------------------

## Helpful hints

- `acc --create_template` will create an example rules file named `template.ac`, with embedded explanations.  
Rename it and edit it, this is a good start point.


- `acc -h` for a [complete list of options](cmd_line.md)


---------------------------------------------------------------------

## Further reading

- [`Acc` fundamental concepts](acc_concepts.md)
- [More on rules and rules files](rules.md)
- [the project genesis](why.md)

---------------------------------------------------------------------
## Help and comments

- Discussions are welcome [here](https://github.com/LionelDraghi/ArchiCheck/discussions)
