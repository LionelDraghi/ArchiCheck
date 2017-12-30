all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

## .PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Component processing in rules unit test"
	@ ${CREATE_PKG} LA.X -in src -with LB.Y spec  
	@ ${CREATE_PKG} LB.Y -in src -with Ada.Containers -with Interfaces.C body 

test1:
	@ #---------------------------------------------------------------------
	${TR} start "Layer component"

	@ ${TR} cmt "Here is the rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules1a.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules1a.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1a >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules1a.txt -I src > output.1a
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1a output.1a"

	@ ${TR} cmt "Let's now add faulty units in LC layer :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules1b.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules1b.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1b >> testrec.md
	@ ${TR} cmt '```'
	
	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "Env component allowed"

	@ ${TR} cmt "Let's add faulty units in an Env component, and allow Env use :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules2.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules2.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules2.txt -I src > output.2
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "Trying to include a unit in more components"

	@ ${TR} cmt "P1 package is in X component, wha if I try to add it to another components?"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules3.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules3.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '> Note that line numbers are false in error messages due to'
	@ ${TR} cmt '> a strange OpenToken bug (I guess).'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.3 >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules3.txt -I src > output.3
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Test on Components embedding components embedding components..."

	@ ${CREATE_PKG} P1 -in dir4 -with Ada.Containers -with Interfaces.C spec

	> rules4.txt
	@ echo "X contains P1"					>> rules4.txt
	@ echo "Y contains X"					>> rules4.txt
	@ echo "Z contains Y"					>> rules4.txt
	@ echo ""								>> rules4.txt
	@ echo "Interfaces.C use is forbidden"	>> rules4.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules4.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules4.txt -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "Error expected, as P1 is unsing a forbidden unit :"
	@ ${TR} cmt '```'
	@ cat expected_output.4a >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules4.txt -I dir4 > output.4a
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4a output.4a"

	@ ${TR} cmt "Lets allow Interfaces.C use through Z components :"
	@ echo ""													>> rules4.txt
	@ echo "Z may use Interfaces.C"						>> rules4.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules4.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules4.txt -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "No more Error expected."

	> expected_output.4b 
	@ ../../Obj/archicheck rules4.txt -I dir4 > output.4b
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4b output.4b"

	@ ${TR} end

clean:
	- ${RM} -rf output.* src *.~ dir?
	@gnat clean -q sub-test
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/rules_on_components.md

