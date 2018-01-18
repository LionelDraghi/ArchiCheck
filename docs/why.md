# Why ArchiCheck?

The idea of ArchiCheck arose during my experience as software achitect on a software product line.
I was frustrated that simple architecture and design decision where not followed.
No matter how clever was the design or the use of the coding language, sooner or later someone was adding a quick and dirty "import" of a compilation unit to easily fix a bug, that was a clear violation of the architecture.
It was not detected a compile time, and sometimes resulted in hard to detect subtle behavior changes, or elaboration order problems.
And, at least, in progressive code spaghettization.

[illustration de la degradation]

So, what could be the solution to this architecture and code degradation?   
The solution was to add the missing semantic. But where?
– Design : ADL, modelling tool and UML extension?
– Code : external tool like ArchiCheck, or directly in future programming languages?
As most of the semantic is already in the code, and beacause there is always code (but not always a model), the decision was to :  
- provide a simple and natural language to describe a software architecture in terms of partition and dependencies;
- have a tool that checks the conformance of sources with one of those description.

Decision was made to have an easy to undestand textual language, that directly state common architecture Metaphors, like :
- _Gtk is a layer over Gdk_
or architecture decisions / elements of architecture, like :
- _My_Component may only use Component_Framework_
- _OS_Lib use allowed only in Hardware_Abstraction_Layer_

One important point is that sometimes a picture is worth a thousand words, sometimes not.  
An architecture diagram hardly beat a simple metaphor.

[illustration]

So, let's keep it simple to use and to understand : no uggly XML or whatever specific cryptic format, no use of shell scripting or programming language, and no graphic tool : just plain english.

And, final decisions for architect piece of mind : 
- the tool will be inserted in the test suite, and continuously enforces code coherency with your design;
- the tool will play an architecture teaching role, by providing clear error messages refering to the faulty code, and to the violated rules. (Am I a lazzy architect? maybe...)

