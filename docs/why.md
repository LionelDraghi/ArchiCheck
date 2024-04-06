---
title: ArchiCheck Genesis
author: Lionel Draghi
date: 21 Pluviôse, an 226
---

# Why ArchiCheck?

The idea of ArchiCheck arose during my experience as software architect on a large software product line.  
I was frustrated that simple architecture and design decisions where not followed : no matter how clever was the design or the use of the coding language, sooner or later someone was adding a quick and dirty "import" of a compilation unit to easily fix a bug, that was a clear violation of the architecture.  

![illustration de la degradation TBD](architecture_degradation.png)

It was not detected a compile time, and sometimes resulted in hard to detect subtle behavior changes, or elaboration order problems.  
And, at least, in progressive code spaghettization.

The obvious solution to this architecture and code degradation was to add the missing semantic.  
But where?

- At design level, using an [ADL](https://en.wikipedia.org/wiki/Architecture_description_language), in modelling tool or UML extension?
- At the code level, with an external tool like ArchiCheck, in special comments needing a preprocessor, directly in future programming languages?

But there is not always a model, and even if so, the code is rarely generated from the model.
On the other hand, there is always code, and most of the semantic is already in the code.  

> So, the first decision was to stay at code level.  

Because I wanted something simple, that don't interfere with existing tools, decision was made :  

> To define a syntax to [describe a software architecture](https://en.wikipedia.org/wiki/Software_architecture_description) in terms of partition and dependencies, in a file independent of sources, and a tool that checks the conformance of sources with that description.

Sometimes a picture is worth a thousand words, sometimes not.  
An architecture diagram hardly beat a simple metaphor.

So, let's keep it simple to use and to understand : no ugly XML or specific cryptic format, no need to use shell scripting or programming language, no "X -> Y" that could be interpreted in many ways, and no graphic tool : just a simple and natural english syntax, that describes : 
- modules

  `Application_Layer contains pkg_1, pkg_2, pkg_3`

- dependencies:

  `pkg_1 may use pkg_2`

- restrictions :
  
  `only Hardware_Abstraction_Layer may use OS_Lib`

- and, for convenience, common architecture Metaphors, like a "Layer" :
  
  `A is a layer over B`

  that is more understandable than the equivalent :

  `only A may use B`
  
  `B cannot use A`


And, final decisions for architect peace of mind :  

The tool will aim at being inserted in the test suite, and constantly remind (or even teach) architecture decisions to the coding team, by providing clear error messages linking the faulty code to the violated architecture rules. 

