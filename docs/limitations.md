Limitations
===========

- only the first compilation unit in Ada and Java files is taken into account.
  As an optimisation, the lexer exit as soon as a compilation unit and
  it's dependencies are identified.

- do not analyze C dependencies embedded in code, only "official" dependencies
  created by (depending on the language) "with", "use", "import", etc.

- detection of visibility through Java fully qualified name ("import"
  is not mandatory in Java to use classes) is **NOT** done at this stage.

- Weird behavior : if all rules involves P childs, P will be reported as not covered by -lnc   
  This is True, but not what is expected... Paradox.

Known Bugs
==========

- localization in rules files is sometimes a mess! (see 07_Rules_Files_Syntax test3)
