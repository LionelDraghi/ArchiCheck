File: Quick Start

About: Archicheck Overview 

Archicheck is a simple tool to describe and enforce architecture/design decision that are not easily checked by languages and compilers.

Archicheck needs :
- a bunch of sources : gives the directories with one or more -I options
- an architecture description : a simple text file describing your architecture

Let's consider for example the file My_Architecture.txt, that describes a simple layered architecture :
> - GUI contains pkg_1, pkg_2
> - DB  contains pkg_3, pkg_4
> - GUI is a layer over DB

Run ArchiCheck that way :
> archicheck -I My_Src_Dir My_Architecture.txt

and it will for example checks that pkg_3 (in the lower layer) is not using pkg_2 (in the upper layer).
(More on what is checked in this case : <Layer rules test suite>)

*And that's it!*

About: Philosophy & status quo

Archicheck is a mockup, with only few (but usefull) functionalities, and not tested on real, big software.
It currently process only Ada sources, but it is meant to accept other languages, and multi-languages projet. 

When I started the project, I wrote a brief description of the project, <available here at http://lionel.draghi.free.fr/ArchiCheck/Archicheck_Overview.pdf>.

About: Download 
A linux excutable is available here : <Download>.

About: Building from sources

*- Get ArchiCheck sources :*

     if you use darcs :
        > darcs clone --lazy https://hub.darcs.net/LioD/ArchiCheck

     otherwise, download a zip archive <here at https://hub.darcs.net/LioD/ArchiCheck/dist> 

     
*- Get OpenToken src :*
 
     download here: <https://github.com/opentoken-io/opentoken.git>

     or do it the Debian way : 
     > apt install libopentoken6-dev gnat make


*- Build :* 

     on linux like plateform :
     > make build

     othewise :
     > gnat make -Parchicheck.gpr

     The exe should be in the Obj directory.
