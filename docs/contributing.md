# Contributing

Comments and issues are welcome [here](https://github.com/LionelDraghi/ArchiCheck/issues/new).
Feel free to suggest whatever improvement.
I'm not a native english speaker. English improvement are welcomes, in this documentation as in the code.
Use cases or features suggestions are also welcome.  

# submitting code / docs

**TBD**

# Architecture Overview

ArchiCheck is a small Ada written software.

All sources are under the Archicheck package :
- ArchiCheck.Main procedure is the controller, in charge (with separate units) of running operation according to the command line analysis;
- ArchiCheck.Rules and child packages defines the rules related types, rules file analisys, rules storage and rules checking.
The (to complex) rules file grammar is implemented in Rules.Parser by using the OpenToken lib;
- ArchiCheck.Lang defines the abstract language parser interface, and child packages define Ada, Java, etc. implementations. Those packages are in charge of sources analysis to find compilation units dependencies.
When a new language processor is added, it shall be registered into Archicheck.Lang.Initialize to be called;
- Settings defines various application wide constant, and stores various parameters set on command line;
- Units is an important (and complex) piece of ArchiCheck, definig Unit, Unit attributes, Dependencies, Unit list. It provides services to store Dependencies (typically during sources parsing in Lang child packages), and to retrieve dependencies or analyse relatioship between Units;
- Sources stores the list of sources found during directories analysis, and defines the location types and services to print this location according to GNU convention;

ArchiCheck was desing to process multiple languages, and AchiCheck vocabulary (Units, Component, Sources, etc.) is as much as possible language agnostic.
Never the less, language semantic is quite different in languages regarding those aspects, and this is an interresting exploration point of the ArchiCheck project.

Note that in the current version, ArchiCheck rely on OpenToken lib for rules file parsing, and for Ada and Java sources parsing. There is no obligation to do so for other language processor as long as the processor comply with the required interface.

# Tests Overview

**TBD**

# Submitting issues

**TBD**