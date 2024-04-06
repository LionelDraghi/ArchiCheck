all: check doc

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

## .PHONY : clean

check: ../../obj/archicheck header test1 test2

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Precedence rules unit test"
	@ ${CREATE_PKG} LA.X -in src -with LB.Y spec  
	@ ${CREATE_PKG} LB.Y -in src -with Ada.Containers -with Interfaces.C -with Interfaces -with Interfaces.Java -with LC.Z -with LA.X -with LB.U spec
	@ ${CREATE_PKG} LC.Z -in src spec

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Declaration of a component already existing in code"

	@ ${TR} cmt "Here is the rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules1.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'But there is already a package named LC.'
	@ ${TR} cmt
	@ ${TR} cmt 'When running :  '
	@ ${TR} cmt "> archicheck rules1.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/archicheck rules1.txt -I src > output.1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Alowing a child of forbidden unit"

	@ ${TR} cmt "Let's forbid Interfaces and allow Interfaces.C"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules2.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> archicheck rules2.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/archicheck rules2.txt -I src > output.2`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* src *.~ 
	@ gnat clean -q sub-test
	@ - ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/precedences_rules.md

