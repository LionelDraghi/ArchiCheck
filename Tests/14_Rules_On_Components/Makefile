all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

## .PHONY : clean

check: ../../Obj/archicheck header test1 test2

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
	@ cat rules1.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules1.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules1.txt -I src > output.1
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} cmt "Let's now add faulty units in LC layer :"
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
	
	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "Env component allowed"

	@ ${TR} cmt "Let's add faulty units in an Env component, and allow Env use :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules3.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules3.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.3 >> testrec.md
	@ ${TR} cmt '```'
	
	@ ../../Obj/archicheck rules3.txt -I src > output.3
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	@ ${TR} end

clean:
	- ${RM} -rf output.* src *.~ 
	@gnat clean -q sub-test
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/rules_on_components.md

