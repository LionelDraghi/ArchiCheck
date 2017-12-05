Contributing
============

Portability:
============

Note that the Ada should be pretty platform independant, but I tested it
only on my Linux/Intel platform. Makefile are clearly not portable, and
probably need heavy refactoring to run on non Unix like system.

Downloading sources:
====================

``git clone https://github.com/LionelDraghi/ArchiCheck.git``

An Archicheck directory will be created.

Otherwise, `go there to download a zip
archive <https://github.com/LionelDraghi/ArchiCheck/archive/master.zip>`__.

Building:
=========

To build ArchiCheck and the tests, just type

``make check``

in the ArchiCheck root directory.

Needed external software : libopentoken, and gnat gcc Ada compiler.

On Debian Linux family :

``apt install libopentoken6-dev gnat make``

archicheck exe will be in the Obj directory.

And to build this documentation / web pages:
============================================

``make doc``

Needed external software : ploticus, NaturalDocs, sloccount and lcov.

On Debian Linux family :

``apt install ploticus naturaldocs sloccount lcov``
