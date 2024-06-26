all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=precedences_rules.md
TR=../../Tools/testrec -o ${MD} 

## .PHONY : clean

check: ../../obj/acc header test1 test2

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
	@ cat rules1.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'But there is already a package named LC.'
	@ ${TR} cmt
	@ ${TR} cmt 'When running :  '
	@ ${TR} cmt "> acc rules1.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1 >> ${MD}
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/acc rules1.txt -I src > output.1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Alowing a child of forbidden unit"

	@ ${TR} cmt "Let's forbid Interfaces and allow Interfaces.C"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules2.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules2.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> ${MD}
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/acc rules2.txt -I src > output.2`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* src *.~ 
	@ gnat clean -q sub-test
	@ - ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}

