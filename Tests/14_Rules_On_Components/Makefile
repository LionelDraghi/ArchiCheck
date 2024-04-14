all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=rules_on_components.md
TR=../../Tools/testrec -o ${MD} 

## .PHONY : clean

check: ../../obj/acc header test1 test2 test3 test4 test5

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Component processing in rules unit test"
	@ ${CREATE_PKG} LA.X -in src -with LB.Y spec  
	@ ${CREATE_PKG} LB.Y -in src -with Ada.Containers -with Interfaces.C body 
	@ ${CREATE_PKG} LC   -in src 

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Layer component"

	@ ${TR} cmt "Here is the rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules1a.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules1a.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1a >> ${MD}
	@ ${TR} cmt '```'
	
	@ ../../obj/acc rules1a.txt -I src > output.1a
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1a output.1a"

	@ ${TR} cmt "Let's now add faulty units in LC layer :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules1b.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules1b.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.1b >> ${MD}
	@ ${TR} cmt '```'
	
	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Env component allowed"

	@ ${TR} cmt "Let's add faulty units in an Env component, and allow Env use :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules2.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules2.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> ${MD}
	@ ${TR} cmt '```'
	
	@ ../../obj/acc rules2.txt -I src > output.2
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Trying to include a unit in more components"

	@ ${CREATE_PKG} P1 -in src 
	@ ${CREATE_PKG} P2 -in src 
	@ ${CREATE_PKG} P3 -in src 
	@ ${CREATE_PKG} P4 -in src 

	@ ${TR} cmt "P1 package is in X component, wha if I try to add it to another components?"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules3.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules3.txt -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "No more error/warning expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '> Note that line numbers are false in error messages due to'
	@ ${TR} cmt '> a strange OpenToken bug (I guess).'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.3 >> ${MD}
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/acc rules3.txt -I src > output.3`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Test on Components embedding components embedding components..."

	@ ${CREATE_PKG} P1 -in dir4 -with Ada.Containers -with Interfaces.C spec

	@ > rules4.txt
	@ echo "X contains P1"					>> rules4.txt
	@ echo "Y contains X"					>> rules4.txt
	@ echo "Z contains Y"					>> rules4.txt
	@ echo ""								>> rules4.txt
	@ echo "Interfaces.C use is forbidden"	>> rules4.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules4.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules4.txt -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "Error expected, as P1 is using a forbidden unit :"
	@ ${TR} cmt '```'
	@ cat expected_output.4a >> ${MD}
	@ ${TR} cmt '```'
	
	@ test ! `../../obj/acc rules4.txt -I dir4 > output.4a`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4a output.4a"

	@ ${TR} cmt "Lets allow Interfaces.C use through Z components :"
	@ echo ""													>> rules4.txt
	@ echo "Z may use Interfaces.C"						>> rules4.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules4.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules4.txt -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "No more Error expected."

	@ > expected_output.4b 
	@ ../../obj/acc rules4.txt -I dir4 > output.4b
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4b output.4b"

	@ ${TR} end

test5:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Test A B C example posted on fr.comp.lang.ada..."

	@ ${TR} cmt '  Except that as "A" is a reserved word, I use X Y Z instead :-)'

	@ ${CREATE_PKG} X.P1 -in dir5 spec 
	@ ${CREATE_PKG} Y.P1 -in dir5 spec -with X.P1
	@ ${CREATE_PKG} Y.P2 -in dir5 spec -with Z.P1
	@ ${CREATE_PKG} Z.P1 -in dir5 spec -with X
	@ ${CREATE_PKG} Z.P2 -in dir5 spec -with Y.P2
	@ ${CREATE_PKG} U    -in dir5 spec -with Z.P2
	@ ${CREATE_PKG} V    -in dir5 spec -with Y.P1

	@ ${TR} cmt 
	@ ${TR} cmt '  +----------++------------+ '
	@ ${TR} cmt '  |     X    ||     Y      | '
	@ ${TR} cmt '  +----------++------------+ '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt '  |           Z            | '
	@ ${TR} cmt '  +------------------------+ '
	@ ${TR} cmt 	 

	@ ${TR} cmt '```'
	@ cat rules5.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Running :  '
	@ ${TR} cmt "> acc rules5.txt -I dir5"
	@ ${TR} cmt 
	@ ${TR} cmt '  With:        Expected:'
	@ ${TR} cmt '  Y.P1 -> X.P1  OK'
	@ ${TR} cmt '  Y.P2 -> Z.P1  OK'
	@ ${TR} cmt '  Z.P1 -> X     Error'
	@ ${TR} cmt '  Z.P2 -> Y.P2  Error'
	@ ${TR} cmt '  U    -> Z.P2  Warning'
	@ ${TR} cmt '  V    -> Y.P1  OK'
	@ ${TR} cmt 

	@ test ! `../../obj/acc rules5.txt -I dir5 | sort > output.5`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.5 output.5"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* src *.~ dir?
	@ gnat clean -q sub-test
	@ - ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
