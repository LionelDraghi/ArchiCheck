Rules File Syntax
=================

Archicheck provide a simple syntax, to describe simple structural aspect of the software architecture.

Layer
-----

Layer are declared thanks to `X is a layer over Y` syntax.

ArchiCheck will ensure that:

- no Y unit is using an X unit (Error msg)
- no unit will jump over X to directly use Y (warning msg)

`X` actually means `X and X.*`, that is all X child's are considered part of X.  
This is why a rules file can be a simple as `Gtk is a layer over Gdk`.

Components declaration
----------------------
A special kind of unit may be declared directly in the rules file, thanks to the `contains` syntax :
```
Presentation_Layer contains Pkg_1, Pkg_2 and Pkg_3;
```
This create a component (a virtual unit), `Presentation_Layer`, that contains other unit 
(`Pkg_1`, `Pkg_2`, etc.), that are also either real unit in the code, or components declared in the 
rules file package.

First Exemple
-------------
```
Applications      contains Browser and Rasterizer
Core_Modules      contains UI_Component, Transcoder, SVG_Generator, Bridge and SVGDOM
Low_Level_Modules contains Renderer, GVT and SVG_Parser

Applications is a layer over Core_Modules
Core_Modules is a layer over Low_Level_Modules
```
This file describes this [Batik architecture](https://xmlgraphics.apache.org/batik/using/architecture.html) :
![](img/Batik.png)

Syntax
------
Rules files syntax is supposed to be as close as possible to natural english, and all the following lines are legal : 

```
Presentation_Layer contains Pkg_1
Presentation_Layer contains Pkg_1;
Presentation_Layer contains Pkg_1.

Presentation_Layer contains Pkg_1, Pkg_2, Pkg_3
Persistence_Layer  contains Pkg_4, Pkg_5 and Pkg_6.
```
Note also that comment are possible, the Ada way, that is starting with `--`.


Environnement packages
----------------------
Some packages are used averywhere in your application, and you don't wan to be flooded by useless warning.
Use the `allowed` syntax :
```
Java use is allowed
```
**Remember that `Java` means also `Java.*`**

or 

```
Ada    use is allowed
System use is allowed
```

Forbidden use
-------------
Symetrically, you may want to ban from you design some packages :
```
Interfaces use is forbidden
```



