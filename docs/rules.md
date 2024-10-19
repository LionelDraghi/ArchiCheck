<!-- omit from toc -->
Rules File
==========

Archicheck rules file uses a simple syntax to describe structural aspects of a software architecture.

This file may contains :  

- [Layer](#layer)
- [Use declaration](#use-declaration)
- [Components declaration](#components-declaration)
- [An example with layers and components](#an-example-with-layers-and-components)
- [Environnement packages](#environnement-packages)
- [Forbidden use](#forbidden-use)
- [Restricted use](#restricted-use)
- [Syntactic sugar](#syntactic-sugar)
- [Comments](#comments)
- [Reserved words](#reserved-words)
- [More complex and open issues](#more-complex-and-open-issues)
  - [Adding virtual units to an existing one](#adding-virtual-units-to-an-existing-one)
  - [Rules precedence](#rules-precedence)

Here is a first example :
```
-- My architecture is layered :
Layer_A is a layer over Layer_B

-- What do I mean by Layer_A and Layer_B? :
Layer_A contains Pkg_A and Pkg_B
Layer_B contains Pkg_C

-- Don't use unbounded containers packages :
Ada.Containers.Indefinite use is forbidden

-- Only the lower level layer can use C external lib :
only Layer_B may use Interfaces.C
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

A layer is a powerful but strict metaphor. 

How can I describe those so common pseudo layered architecture, where the upper layer can access whatever is below?

Here is the [Gtk+](https://www.gtk.org/overview.php) example :![](tests/gtk.png)

The `X may use Y` provides a more relax rule than the layer one.  
ArchiCheck will just ensure that:

- no Y unit is using an X unit (Error msg)

For example here:

```
Pango may use Cairo
Pango may use Glib
Gdk   may use Cairo
-- etc.
```
A more complete GtkAda possible rules file is available in tests, [here](tests/gtkada.md).

Components declaration
----------------------

But what if there is no such `X` or `Y` packages in code, if those just exist in some architecture description?
A special kind of unit may be declared directly in the rules file, thanks to the `contains` syntax :
```
Presentation_Layer contains Pkg_1, Pkg_2 and Pkg_3;
```
This create a component (a virtual unit), `Presentation_Layer`, that contains other unit 
(`Pkg_1`, `Pkg_2`, etc.), that are also either real unit in the code, or components declared in the rules file package.

Now, `Presentation_Layer` can be used in rules as if it was a real compilation unit.

An example with layers and components
-------------------------------------

Let's consider this [Batik architecture](https://xmlgraphics.apache.org/batik/using/architecture.html) :  
![](Batik.png)

It could be described this way :

```
Applications      contains Browser and Rasterizer
Core_Modules      contains UI_Component, Transcoder, SVG_Generator, Bridge and SVGDOM
Low_Level_Modules contains Renderer, GVT and SVG_Parser

Applications is a layer over Core_Modules
Core_Modules is a layer over Low_Level_Modules
```

Environnement packages
----------------------

Some packages are used everywhere in your application, and you don't want to be flooded by useless warnings.

Use the `allowed` syntax :
```
Java.IO use is allowed
```
> [!TIP]
> Remember that `Java.IO` actually means `Java.IO` and `Java.IO.*`

Forbidden use
-------------

Symmetrically, you may want to ban from your design some packages :
```
Interfaces.C use is forbidden
```

Restricted use
--------------

Prefixing a normal `may use` rule with `only` make it possible to limit accesses to some specific unit :

```
only Low_Level_Layer may use Interfaces.C
```
A restricted `use` will check that, like a normal `use`, `Interfaces.C` is not using `Low_Level_Layer`, but also that no other unit is using `Interfaces.C`.

Note that multiple restricted or non restricted `use` may apply cumulatively to a unit :
```
only X may use P1
only Y may use P1
Z may use P1
-- is equivalent to :
--   only X, Y and Z may use P1
-- that is not (yet) a legal `Acc` syntax
```

Syntactic sugar
--------------

Rules files syntax is supposed to be as close as possible to natural english, with a flexible syntax. Rules line may terminate with semicolon or nothing, and units may be separated with `and` or comma (and not yet blanks, curiously).

All the following lines are legal : 

```
-- Component declaration can be made in several lines :
Presentation_Layer contains Pkg_1
Presentation_Layer contains Pkg_2  

\\ Or in a more compact way :
Presentation_Layer contains Pkg_1, Pkg_2, Pkg_3 
Presentation_Layer contains Pkg_4, Pkg_5 and Pkg_6
```

Comments
--------

As you may have noticed in the previous example, comments are possible in several formats (but single line comments only), so keep your own habit :

- the Ada way, that is lines starting with `--`, 
- the Java / C# way, lines starting with  `//`, 
- the Shell way, lines starting with  `#`.

(Those format seems to be the most popular for single line comments in programming languages, according to [Rosetta Code](https://rosettacode.org/wiki/Comments))

Reserved words
--------------

Words used in rules syntax are reserved, and can't be used as unit name in rules file (but obviously no problem to use them as file or compilation unit name) : 

1. `a`
1. `Allowed`
1. `and`
1. `contains`
1. `Forbidden`
1. `is`
1. `layer`
1. `may`
1. `only`
1. `over`
1. `use`
1. `Forbidden`

More complex and open issues
----------------------------

### Adding virtual units to an existing one

What if I declare a virtual unit while there is an existing unit with the same name?  
For example, if there is a `Utility` compilation unit, with child units, and I declare in the rules file :
```
Utility contains Glib and Ada.Containers
```
This is is considered as an "Add" operation : `Glib`, `Ada.Containers` and there child packages should be considered as if there where actually `Utility.Glib` and `Utility.Ada.Containers`.
And obviously, the existing `Utility` compilation unit is also part of this `Utility` component.

### Rules precedence

What if I want to prevent a unit use, except for a specific child?

For example : 
```
Interfaces   use is forbidden
Interfaces.C use is allowed
```

Whatever the order of those two declarations, it should have this behavior :

- `Interfaces.C` and all it's child units are allowed
- all other `Interfaces` child are forbidden
- and obviously `Interfaces` use is forbidden

> [!IMPORTANT]
> **Note that "allowed" as the precedence over "forbidden", meaning that if you 
explicitly allow some unit, you can't forbid one of his child.**

For example, if we invert the previous example : 
```
Interfaces   use is allowed
Interfaces.C use is forbidden
```
the second line will be ignored.

> Implementing a more logical behavior seem's complex.   
> But who knows, I may change my mind, so consider this as likely to change.  
> Any comment on that feature is welcome. 

