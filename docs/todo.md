TODO list
=========

Features:
---------

- Implement a command line way to pass simple rules
- Implement globbing characters, for example `only *.Porting_Kit can use X`

Ada specifics:
--------------

- Exclude "limited with" from dependencies (or consider it as warning?)
- use Type processing
- Processing Import pragma as a dependency. e.g.
  
  ```
  pragma Import (C, Get_File_Names_Case_Sensitive,
                 "__gnat_get_file_names_case_sensitive");   
  ```

- Implement the _only X can use pragma import_ rule

Java specifics:
---------------

- detection of visibility through Java fully qualified name ("import"
  is not mandatory in Java to use classes)

Development environment:
------------------------

- update NaturalDocs with markdown and mkdocs in the greatings
- decide for file casing!
- Implement a git connected version id in the source and helper
- Have the tests OS agnostics (test depending on the path of Ada file analyzed, that fail   if runned on Windows)
- And why not, have the test runned on each target plateform automaticaly (how to do that?  Docking, VM...) 
- Consider replacement of the documentation tools (Dia for UML diagrams, NaturalDocs,  
  etc.)
  for NaturalDocs : https://daringfireball.net/projects/markdown/, reST/sphinx http://www.sphinx-doc.org/en/stable/, http://pandoc.org/index.html
- Use Livecoding? :-) (https://www.livecoding.tv/livestreams/)
- considering flatpack / snap / AppImage / OrbitalApps to distribute archicheck


Documentation:
--------------

- Complete the 2004 slides! :-)
- build a pdf doc based on the md files.
- insert in the doc a tab describing for each langage what is checked, and what is not.

Done:
-----

- v0.5.5 : add localisation in src and rules files in error messages
- v0.5.4 : fixed bug in Java OpentToken lexer, that cause infinite loop on JavaDocs comments 
- Removing colun in error message (useless)
- Implement an error return code when the rules are broken, to allow a normal use in Makefiles
- mv from NaturalDocs to md format, generate an home pag with a less obsolet look.
- Change/Simplify the various options to know what the tool understand from the rules 
  file, and from analyzed sources 
- Use [gnu error messages format]( https://www.gnu.org/prep/standards/html_node/Errors.html) (_sourcefile:lineno: message_) 
  when identifying a rule that breaks
- Update the use of gnat (compiler option, gnatmake replacement))
- Regroup test on Syntaxic sugar in the rule files, that is to show insensitivity to 
  casing, "," ";" "." Separators, comments, etc.
- Update the Rules syntax text file to show all syntaxic sugar possibilities (use of "." 
  / ";" / "," / "and", etc.), and to show how to comment
- Restructure the home page, to have a short intro and a "quick run" section that 
  summarize "Installing' and "Getting started", and split the rest in other text files.
- Complete the link between NaturalDocs and Tests result, by adding a Result keyword 
