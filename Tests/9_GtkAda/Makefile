all: check

TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3

header:
	${TR} create "GtkAda tests suite"

test1:
	${TR} start "File Identification"		
	
	@ ls -1 /usr/share/ada/adainclude/gtkada/*.ad[sb]		| sed "s/.*\///" | sort	> expected_output.1
	@ ../../Obj/archicheck -q -lf -I /usr/share/ada/adainclude/gtkada	| sed "s/.*\///" | sort	> output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end

test2:
	${TR} start "Unit Identification"		
	
	# to do a real check here, I need as a reference an ASIS tools that list all dependency
	@ ../../Obj/archicheck -ld -I /usr/share/ada/adainclude/gtkada > output.2
	# TBD ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	${TR} start "Testing layers"		
	
	${TR} cmt "Checking those rules over GtkAda:"							
	${TR} cmt "(start code)"							
	@ cat rules.txt			>> testrec.txt						
	${TR} cmt "(end)"
								
	@ ../../Obj/archicheck -q rules.txt -I /usr/share/ada/adainclude/gtkada	> output.3
	# -q is set to avoid warnings and focus on error
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end


clean:
	- ${RM} -rf output.? expected_output.[12]
	${TR} clean
