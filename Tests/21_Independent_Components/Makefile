all: clean check doc

CREATE_PKG=../../Tools/create_pkg
MD=independent_components.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Independent Component test suite"

test1: clean
	@ #---------------------------------------------------------------------
	@ ${TR} start "Test Independent Components"

	@ ${CREATE_PKG} X.P1 -in dir1 spec  
	@ ${CREATE_PKG} X.P2 -in dir1 spec -with X.P1 -with Bus
	@ ${CREATE_PKG} Y    -in dir1 spec -with Bus
	@ ${CREATE_PKG} Bus  -in dir1 spec
	@ ${CREATE_PKG} U    -in dir1 spec -with X.P2
	@ ${CREATE_PKG} V    -in dir1 spec -with Y

	@ ${TR} cmt 
	@ ${TR} cmt '  +---------+  +-----------+ '
	@ ${TR} cmt '  |    X    |  |     Y     | '
	@ ${TR} cmt '  +---------+  +-----------+ '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt '  |          Bus           | '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt 	 

	@ > rules1.txt
	@ echo "X and Y are independent"		>> rules1.txt

	@ ${TR} cmt '```'
	@ cat rules1.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules1.txt -I dir1"
	@ ${TR} cmt 
	@ ${TR} cmt '  With:        Expected:'
	@ ${TR} cmt '  X.P2 -> X.P1  OK'
	@ ${TR} cmt '  X.P2 -> Bus   OK'
	@ ${TR} cmt '  Y    -> Bus   OK'
	@ ${TR} cmt '  U    -> X.P2  OK'
	@ ${TR} cmt '  V    -> Y     OK'
	@ ${TR} cmt 

	@ test ! `../../obj/acc rules1.txt -I dir1 | sort > output.1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end

test2: 
	@ #---------------------------------------------------------------------
	@ ${TR} start "Test broken Independent Components rule"

	@ ${CREATE_PKG} Y.P3 -in dir1 spec -with X.P1

	@ ${TR} cmt 
	@ ${TR} cmt '  +---------+  +-----------+ '
	@ ${TR} cmt '  |    X    |  |     Y     | '
	@ ${TR} cmt '  +---------+  +-----------+ '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt '  |          Bus           | '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt 	 

	@ ${TR} cmt '```'
	@ cat rules1.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules1.txt -I dir1"
	@ ${TR} cmt 
	@ ${TR} cmt '  With:        Expected:'
	@ ${TR} cmt '  X.P2 -> X.P1  OK'
	@ ${TR} cmt '  X.P2 -> Bus   OK'
	@ ${TR} cmt '  Y    -> Bus   OK'
	@ ${TR} cmt '  U    -> X.P2  OK'
	@ ${TR} cmt '  V    -> Y     OK'
	@ ${TR} cmt '  Y.P3 -> X.P1  Error'
	@ ${TR} cmt 

	@ test ! `../../obj/acc rules1.txt -I dir1 | sort > output.2`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* src *.~ dir?
	@ gnat clean -q sub-test
	@ - ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
	

