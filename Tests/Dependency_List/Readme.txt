File: --list_dependencies option test

This test check the dependency list extraction from simple Ada sources.

The command line is :
> ./archicheck -I SOURCES_DIRECTORY --list_dependencies

The output will be of this kind :

(start code)

A specification depends on B
A body          depends on C
C specification depends on B
C specification depends on F
C.D specification depends on E.G.H

(end)


