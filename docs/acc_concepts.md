<!-- omit from toc -->
# **Fundamentals `Acc` concepts**

- [1. Introduction](#1-introduction)
- [2. Compilation Units](#2-compilation-units)
- [3. `Acc` Architectural Description Language](#3-acc-architectural-description-language)
- [4. Components](#4-components)
- [5. Visibility](#5-visibility)
- [6. Banned](#6-banned)


## **1. Introduction**

`Acc` is manipulating concepts that are heavily used and overloaded, within both the architect and the coding language worlds.
Therefore, it is useful to give some definitions here. 

`Acc` is dedicated to the description of the static structure of software. 
It's all about [Components](#components) and [Visibility](#visibility).

`Component` is intended here as a clustering of other Components and `Compilation units` (in short `Units`).

## **2. Compilation Units**
`Compilation Units` is a language dependent concept, but generally represent an autonomously compilable piece of code, that :  

1. encapsulates a set of related functions or data, 

2. provide an interface through which it can be used,  

3. and is dependent on other `Compilation Units`.  

`Compilation Units` are by definition compilable and generate a single object.

`Compilation units` are defined in `Files`. 
Depending on languages and conventions, a single file may contain one ore more `Units`, and a `Unit` may have an `interface` part and an `implementation part` in different files. 

How languages concepts match `Acc` concepts is [further detailed here](languages_concepts.md).


## **3. `Acc` Architectural Description Language**

`Acc` provides an Architectural Description Language, that differs from most existing one, because it aims at being as close at possible as human architect parlance :

- it is **written in plain english**, 
  
  `HAL contains Linux_Interface and Windows_Interface`
  
- and **uses common architecture pattern metaphor**, like :
  
  `Presentation_Layer is a layer over Business_Layer`  

  You can't beat that [expressiveness power](https://en.wikipedia.org/wiki/Expressive_power_\(computer_science\))  
  As simple as that.
  
- second, it is not only about what is obvious in the diagrams, but **also about what is forbidden** :
  
  `only HAL may use Linux_Interface`
  
  From my architect experience, I learned that this is a very important point.

- third, it is smart enough to stay **simple for user**. 
  
  Lets consider this :  
  
  `L1 is a layer over L2`  
  
  Like a human, `Acc` is smart enough to adapt this statement to several situations :  

  - there is an actual compilation unit named L1,  

  - there is no actual compilation unit named L1, but there is L1 directory containing Units, or a bunch of files prefixed with L1,  

  - there is no concrete L1 at all, and `Acc` will conclude that it's an abstract component.  
  
  And obviously, same for L2. 

In a nutshell, `Acc` Architecture Description Language is designed to be simple for human, not for XML parsers.

(And, unfortunately, simple for user sometimes do not always means simple for implementation!)

> [!NOTE]  
> All this may seems too "informal", but actually there is no ambiguity.  
> You don't always need a grammar description and a complex XML file to describe things unambiguously.  
> **This is the Acc bet!**

In `Acc` Architecture Description Language, some lines are more **rules lines** (`Interfaces.C use is forbidden`) and other are **descriptions lines** (`A contains B, C and D`).  
Nevertheless, in `Acc` vocabulary, the description file is called the `rules file`.


## **4. Components**

Like `Compilation Units`, `Components`  
- exposes an interface
- contains internal components
- may be be dependent of others modules

### Virtual components
  
Components appearing in the architecture or design descriptions do not always materialize through the code. 
Archicheck offers the possibility to explicitly declare such a component that is not defined in the code.
It is then called a virtual component.

Sometimes, a directory gathering source code files materialize the component name (all Gdk sources are in the Gdk subdirectory), or there is a naming convention (All Gdk file name or compilation unit start with Gdk).

## **5. Visibility**


## **6. Banned**

- [Module](https://en.wikipedia.org/wiki/Modular_programming) : module are from a design point considered as small `Components`, but may be a language concept also (synonym to Ada package, for example).
  Archicheck make no difference between Modules and Components. Both are composable. If A contains B and C, Archicheck is not concerned in your decision to call A a component, B and C modules, or to call them all modules or components.
  As it was felt more general, and less linked to specific programming languages, Archicheck uses the word `component`.

  For the sake of clarity, `Acc` parlance will avoid it, and use either `Component` for the first, or `Compilation Unit` (or the shorter `Unit`) for the later.

