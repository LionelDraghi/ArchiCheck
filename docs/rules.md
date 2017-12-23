Rules File Syntax
=================

Archicheck rules file uses a simple syntax to describe simple structural aspect of the software architecture.

This file may contains :
- Comments
- [Layer declaration](#layer)
- [Component declaration](#components-declaration)
- [Allowed use declaration](#environnement-packages)
- [Forbidden use declaration](#forbidden-use)
- [Restricted use declaration](#restricted-use)
- [Use declaration](#use-declaration)             

Here is a first example :
```
-- My architecture is layered :
Layer_A is a layer over Layer_B

-- What do I mean by Layer_A and Layer_B? :
Layer_A contains Pkg_A and Pkg_B
Layer_B contains Pkg_C

-- Don't depend on unbounded containers packages! :
Ada.Containers.Indefinite use is forbidden
```

Layer
-----

Layer are declared thanks to `X is a layer over Y` syntax.

ArchiCheck will ensure that:

- no Y unit is using an X unit (Error msg)
- no unit will jump over X to directly use Y (Warning msg)

> Note that `X` actually means `X and X.*`, that is all X child's are considered part of X.  
This is why a rules file can be a simple as `Gtk is a layer over Gdk`.

Use declaration
---------------

A layer is a powerfull but strict metaphore.  
But how can I describe that so common pseudo layred architecture, where the upper layer can access whatever is below?  
Here is the [Gtk+](https://www.gtk.org/overview.php) example :![](tests/gtk.png)

The `X use Y` provides a more relax rule.  
ArchiCheck will just ensure that:

- no Y unit is using an X unit (Error msg)

For example here:

```
Pango use Cairo
Pango use Glib
Gdk use Cairo
-- etc.
```
A more complete GtkAda possible rules file is available in tests, [here](#tests/gtkada.md).

Components declaration
----------------------

But what if there is no such `X` or `Y` packages in code, if those just exist in some architecture description?
A special kind of unit may be declared directly in the rules file, thanks to the `contains` syntax :
```
Presentation_Layer contains Pkg_1, Pkg_2 and Pkg_3;
```
This create a component (a virtual unit), `Presentation_Layer`, that contains other unit 
(`Pkg_1`, `Pkg_2`, etc.), that are also either real unit in the code, or components declared in the 
rules file package.
Now, `Presentation_Layer` can be used in rules like if it was a real compilation unit.

First Exemple
-------------

Let's consider this [Batik architecture](https://xmlgraphics.apache.org/batik/using/architecture.html) :  
![](img/Batik.png)

It could be described this way :

```
Applications      contains Browser and Rasterizer
Core_Modules      contains UI_Component, Transcoder, SVG_Generator, Bridge and SVGDOM
Low_Level_Modules contains Renderer, GVT and SVG_Parser

Applications is a layer over Core_Modules
Core_Modules is a layer over Low_Level_Modules
```

Syntax
------

Rules files syntax is supposed to be as close as possible to natural english, and all the following lines are legal : 

```
Presentation_Layer contains Pkg_1
Presentation_Layer contains Pkg_2 -- Component declaration can be made in several lines 

-- Or in a more compact way :
Presentation_Layer contains Pkg_1, Pkg_2, Pkg_3 
Persistence_Layer  contains Pkg_4, Pkg_5 and Pkg_6
```
Note also that comment are possible, the Ada way, that is starting with `--`.

Environnement packages
----------------------

Some packages are used everywhere in your application, and you don't wan to be flooded by useless warning.

Use the `allowed` syntax :
```
Java use is allowed
```
**Remember that `Java` actually means `Java.*`**

or 

```
Ada    use is allowed
System use is allowed
```

Forbidden use
-------------

Symetrically, you may want to ban from you design some packages :
```
Interfaces.C use is forbidden
```

Restricted use
--------------

A less definitive rules make it possible to limit access to some unit to some other units :

```
only Low_Level_Layer may use Interfaces.C
```

More complex issues
-------------------

### Adding virtual units to an existing one

What if I declare a virtual unit while there is an existing unit with the same name?  
For example, if there is a `Utility` compilation unit, with child units, and I declare in the rules file :
```
Utility contains Glib and Ada.Containers
```
Two possible behavior here :  
- this is illegal and should be dealt with as an error when reading the rules file
- this is is considered as an "Add" operation : `Glib`, `Ada.Containers` and there child packages should be considered as if there where actually `Utility.Glib` and `Utility.Ada.Containers`.
  
As I am writing this (v0.4 Christmas 2017), I'm trying the second way, altrough it smell very complex (and I hate that precise smell). 
> Consider this as TBD.  
> Any comment on that feature is welcome! 

### Rules precedence

What if I want to prevent a unit use, except for a specific child?

For example : 
```
Interfaces   use is forbidden
Interfaces.C use is allowed
```

Whatever the order of those two declarations, it should have this behaviour :
- `Interfaces.C` and all it's child units are allowed
- all other `Interfaces` child are forbidden

But, until globbing character implementation, there is no way to do an explicit choice for `Interfaces`.

The "future" solution will be :
```
Interfaces.* use is forbidden 
Interfaces.C use is allowed
Interfaces   use is allowed -- or forbidden, according to your choice
```
