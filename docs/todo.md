TODO list
=========

Features:
---------

-       Implement a command line way to pass simple rules
-       Implement globbing characters, for example `only *.Porting_Kit can use X`
-       Add localisation in src and rules files in error messages

Ada specifics:
--------------

-       Exclude "limited with" from dependencies (or consider it as warning?)
-       use Type processing
-       Processing Import pragma as a dependency. e.g.
  
   	```
        pragma Import (C, Get_File_Names_Case_Sensitive,
   	               "__gnat_get_file_names_case_sensitive");   
   	```

-  	Implement the _only X can use pragma import_ rule

Java specifics:
---------------

- 	detection of visibility through Java fully qualified name ("import"
	is not mandatory in Java to use classes)

Development environment:
------------------------

- 	update NaturalDocs with markdown and mkdocs in the greatings
-	decide for file casing!
-       Implement a git connected version id in the source and helper
-       Have the tests OS agnostics (test depending on the path of Ada file analyzed, that fail if runned on Windows)
-	And why not, have the test runned on each target plateform automaticaly (how to do that? Docking, VM...) 
-       Consider replacement of the documentation tools (Dia for UML diagrams, NaturalDocs, etc.)
        for NaturalDocs : https://daringfireball.net/projects/markdown/, reST/sphinx http://www.sphinx-doc.org/en/stable/, http://pandoc.org/index.html
-       Use Livecoding? :-) (https://www.livecoding.tv/livestreams/)
- 	considering flatpack / snap / AppImage / OrbitalApps to distribute archicheck

Bug:
----

- 	bug in Java OpentToken lexer, that cause infinite loop on JavaDocs comments 
-       localization in rules files is a mess! (see 07_Rules_Files_Syntax test3

Documentation:
--------------

-       Complete the 2004 slides! :-)
-	mv from NaturalDocs to md format, generate an home pag with a less obsole look, and build a pdf doc based on the same md files.
- 	insert in the doc a tab describing for each langage what is checked, and what is not.

Done:
-----

-       v0.4.2 : Auto add -help output to the doc thanks to Makefile- 	implement comments in rules files
-       v0.4.2 : Implement the _only X can use Y_ rule
-       v0.4.1 : implemented `only Porting_Kit may depends on C_Files`
-       v0.4.1 : implemented a _may_ semantics : `GLU may depends on GL`
-       v0.4.1 : taking into account other Ada unit than package : procedure, protected object, task, etc.
-       Change/Simplify the various options to know what the tool understand from the rules file, and from analyzed sources 
-       Use <gnu error messages format at https://www.gnu.org/prep/standards/html_node/Errors.html> ( _sourcefile:lineno: message_ ) when identifying a rule that breaks
-       Implement an error return code when the rules are broken, to allow a normal use in Makefiles, unless -no-error
-       Update the use of gnat (compiler option, gnatmake replacement))
-       Regroup test on Syntaxic sugar in the rule files, that is to show insensitivity to casing, "," ";" "." Separators, comments, etc.
-       Update the Rules syntax text file to show all syntaxic sugar possibilities (use of "." / ";" / "," / "and", etc.), and to show how to comment
-       Restructure the home page, to have a short intro and a "quick run" section that summarize "Installing' and "Getting started", and split the rest in other text files.
-       Complete the link between NaturalDocs and Tests result, by adding a Result keyword 

 

 
