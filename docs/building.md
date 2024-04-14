<!-- omit from toc -->
Building 
========

Downloading a Linux exe :
-------------------------

A Linux exe is available [here](download.md).

Downloading sources :
---------------------

With `git`: `git clone https://github.com/LionelDraghi/ArchiCheck.git`

An Archicheck directory will be created.

Otherwise, [go there to download a zip archive](https://github.com/LionelDraghi/ArchiCheck/archive/master.zip). 

Building :
----------

To build ArchiCheck, just run `make build` in the root directory.

Needed external software : gnat gcc Ada compiler. 
  
> On Debian Linux family : `apt install gnat make`  

 > [!NOTE] : libopentoken6.1-dev is no more needed. 
 > To ease building on non Debian platform, I integrated OpenToken sources.

archicheck exe will be in the `Obj` directory.

To run ArchiCheck tests : `make check`
> Warning : tested on a Linux platform only
  
Portability :
-------------

Note that the Ada code should be pretty platform independent, but it is currently only tested on a Linux/Intel platform.
  
Makefiles are clearly not portable, and obviously need heavy refactoring to run on non Unix systems. 




