Building 
========

Downloading a Linux exe :
-------------------------

A Linux exe is available [here](download.md).

Downloading sources :
---------------------

  `git clone https://github.com/LionelDraghi/ArchiCheck.git`

  An Archicheck directory will be created.

  Otherwise, [go there to download a zip archive](https://github.com/LionelDraghi/ArchiCheck/archive/master.zip). 

Building :
----------

  To build ArchiCheck, just run `make build` in the root directory.

  Needed external software : libopentoken, and gnat gcc Ada compiler. 
  
  > On Debian Linux family : `apt install libopentoken6-dev gnat make` to install.

  archicheck exe will be in the `Obj` directory.

  To build ArchiCheck and test it : `make check`.
  > Warning : tested on a Linux platform only
  
Portability :
-------------

  Note that the Ada code should be pretty platform independant, but I tested it only on my Linux/Intel platform.
  
  Makefiles are clearly not portable, and obviously need heavy refactoring to run on non Unix like system. 

To build this documentation / web pages :
---------------------------------------------

  `make doc`
  > Warning : tested on a Linux platform only
  
  Needed external software : ploticus, NaturalDocs, sloccount and lcov. 

  > On Debian Linux family : `apt install ploticus mkdocs sloccount lcov`




