# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/)
and version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- [Unreleased]
  > - [Added] new rule `X, Y, Z are independent` created
  > - [Added] ArchiCheck is now an Alire crate
  > - [Changed] OpenToken src removed, and added as an Alire dependency
    

- [0.5.8] - 2024-04-06
  > - [Changed] The -lf / --list_files output is now sorted by language and then by file name
  > - [Added] First C language "quick and dirty" implementation

- [0.5.7] - 2018-02-14
  > - [Added] `-Ir` option, equivalent to `-r -I`
  > - [Changed] no more reference to an external OpenToken lib, OpenToken sources are now integrated.

- [0.5.6] - 2018-01-14
  > - [Added] `-ct` option to create a rules file template 
  > - [Added] checking of rules that do not involve any source
  > - [Fixed] Java annotations processing bug fix

- [0.5.5] - 2018-01-08
  > - [Added] First `--list_non_covered` implementation
  > - [Fixed] Java lexer buffer size increased, can now process comment of more 1024 chars (till 8192)

- [0.5.4] - 2018-01-05
  > - [Added] `[only] X may use` now apply to a unit list

- [0.5.2] - 2018-01-03
  > - [Added] Warning as error `-We` processing added
  > - [Added] Error exit status in case of rules error
  > - [Fixed] bug fix in Is_Unit_In_Component

---

# Previous history:

- january 2018 : v0.5.x  

- november / december 2017 : v0.4
  >  - Java processing added, and test on Batik code
  >  - `-r` option added (recursive `-I`) 
  >  - more complete Ada unit processing (protected, task, separate)
  >  - new semantics : `is forbidden`, `is allowed`, `[only] may use`,  
  >    cf. [rules file tutorial](rules.md)
  >  - Ada, Java and Shell style comments added to rules files
  >  - web site migration from NaturalDocs to Markdown done
  >  - release as v.0.5  
- october 2017  : v0.3 version
  >  - OpenToken is now used for rules file analysis
  >  - First "real-life" test on Gtk-Ada
  >  - Starting Ada 2012 use
- june 2017     : first public release, mockup code 
- 2005 - 2017   : glacial period!
- 2004 - 2005   : initial code and announce on comp.lang.ada, [in this thread](http://groups.google.com/group/comp.lang.ada/browse_thread/thread/4a195a443fce793e/41bb2cb527464bab?q=comp.lang.ada+example+of+layered+software#41bb2cb527464bab).
