all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Use rules test suite"


test1:
	@ #---------------------------------------------------------------------
	${TR} start "A component is using another one"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3
	@ ${CREATE_PKG} P2 body  -in dir1 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1
	@ ${CREATE_PKG} P4 spec  -in dir1

	@ ${TR} cmt "Code compliant with the rules, should be OK"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ > expected_output.1
	@ ../../Obj/archicheck -q -I dir1 rules.1 > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end

clean:
	- ${RM} -rf output.* dir? dir?? *.~
	${TR} clean

