File: ArchiChech Readme

About: Archicheck Overview

Archicheck is a simple tool to describe and enforce architecture/design decision that are not easily checked by languages and compilers.

Archicheck needs :

a bunch of sources - gives the directories with one or more -I options
an architecture description - a simple text file describing your architecture. 

Let's consider for example the file My_Architecture.txt, that describes a simple layered architecture :
> - GUI contains pkg_1, pkg_2
> - DB  contains pkg_3, pkg_4
> - GUI is a layer over DB

Run ArchiCheck that way :
> archicheck -I My_Src_Dir My_Architecture.txt

and it will for example checks that pkg_3 (in the lower layer) is not using pkg_2 (in the upper layer).
(More on what is checked in this case : <Layer rules tests>)

*And that's it!*

About: Philosophy
What I had in mind when I created this tools is briefly (and partly) explained on those slides <http://lionel.draghi.free.fr/ArchiCheck/Doc/Archicheck Overview.pdf>

Note that this is a draft, incomplete, and with an especially sober style :-)

About: Installing 

Portability:
Note that the Ada should be pretty platform independant, but I tested it only on my Linux/Intel platform.
Makefile are clearly not protable, and probably need heavy refactoring to run on non Unix like system.

Downloading:
If your're an (happy) darcs user :
> darcs clone --lazy https://hub.darcs.net/LioD/ArchiCheck

An Archicheck directory will be created.

Otherwise, use the ~download .zip~ button on the right of the screen.   

Building :
To build ArchiCheck and the tests, just type 
> make 
in the ArchiCheck root directory.

Needed external software : libopentoken, and gnat gcc Ada compiler. 
On Debian Linux family :
> apt install libopentoken6-dev gnat make 

archicheck exe will in the Obj directory.

To update this documentation:
> make doc

Needed external software : ploticus, NaturalDocs and sloccount. 

On Debian Linux family :
> apt install ploticus naturaldocs sloccount 

To contribute: 
The easiest way to contribute (at this stage) is to use darcs, and to submit darcs patches, whatever is changed or added in the code, tests, text file, etc.

The shortest tuto: 
> darcs clone --lazy https://hub.darcs.net/LioD/ArchiCheck  # should be already done
hack code / tests / doc!
> darcs record -a -m "what my patch does"
> darcs send -o patch_file
send the generated patch_file <to ArchiCheck maintener at lionel.draghi@free.fr>

More on working with darcs:
Getting started with darcs : <http://darcs.net/QuickStart>.

Any idea or feedback is also welcome <here at lionel.draghi@free.fr>.

About: Status Quo:
Archicheck is a mockup, with only few functionnality (but essential one), and not really tested on real, big software.
 
It currently process only Ada sources, but is designed to accept other languages, and multi-languages projet.

(This objective resulted for exemple in the OpenToken package adoption).


I stopped all software developpement related activities in december 2005, just after having kind of announced my tool on comp.lang.ada, <in this thread at http://groups.google.com/group/comp.lang.ada/browse_thread/thread/4a195a443fce793e/41bb2cb527464bab?q=comp.lang.ada+example+of+layered+software#41bb2cb527464bab>.


Since then, I just updated the various software involved (gnat, NaturalDocs, OpenToken, etc.), added licence (GPL) and a README.

I also commented out my tentative to use OpenToken for the rules file analysis (OpenToken is still used Ada sources analysis). 

(Current implementation is in the Analyze_Rules separate procedure, OpenToken version is in the Analyze_Rules_File.* separate procedures). 

As I was not able to complete this when I was daily involved in programming, I probably won't do it now without help!


More generally speaking, I won't be that much available for extending this tool, but I'll try to support anyone interested in. 


About: About

Version :
TBD

License:
ArchiCheck is distributed under GPL version 3, refer to the COPYING file.

Author & contact:
ArchiCheck was created by Lionel Draghi (<email at mailto:lionel.draghi@free.fr>).

Ada:
ArchiCheck is proudly written in Ada.
<http://getadanow.com>


Special thanks to:
- <Ludovic and all the Ada in Debian team at https://people.debian.org/~lbrenta/debian-ada-policy.html#Introduction>. 
- <darcs at http://darcs.net/>
- <NaturalDocs at http://www.naturaldocs.org/>
- <OpenToken at http://stephe-leake.org/ada/opentoken.html>
- <GNAT & GPS at http://libre.adacore.com/tools/gps/>



