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
	${TR} start "Use"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3
	@ ${CREATE_PKG} P2 body  -in dir1 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1
	@ ${CREATE_PKG} P4 spec  -in dir1

	@ ${TR} cmt "First test, should be OK"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ > expected_output.1
	@ ../../Obj/archicheck -q -I dir1 rules.1 > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "Reverse dependency test"
	@ ${CREATE_PKG} P1 spec  -in dir2 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir2 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir2
	@ ${CREATE_PKG} P4 spec  -in dir2 -with P1
	@ ${CREATE_PKG} P5 spec  -in dir2 -with P4
	
	@ ${TR} cmt "Detection of a dependancy from a used component to the using component."
	@ ${TR} cmt "(start code)"
	cat rules.2 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.2 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck --quiet -I dir2 rules.2 > output.2
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "Layer bridging test"
	@ ${CREATE_PKG} P1 spec  -in dir3a -with P2
	@ ${CREATE_PKG} P2 spec  -in dir3a
	@ ${CREATE_PKG} P2 body  -in dir3a -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir3b
	@ ${CREATE_PKG} P4 spec  -in dir3b
	@ ${CREATE_PKG} P6 spec  -in dir3c
	@ ${CREATE_PKG} P6 body  -in dir3c -with P4

	@ ${TR} cmt "Detection of a dependancy link crossing a layer."
	@ ${TR} cmt "(see test_3.png)"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.3 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -I dir3a -I dir3b -I dir3c rules.1 > output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Using a package that is neither in the same layer, nor in the visible layer"
	@ ${CREATE_PKG} P1 spec  -in dir4 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir4 -with P3 -with P4 -with P7
	@ ${CREATE_PKG} P3 spec  -in dir4
	@ ${CREATE_PKG} P4 spec  -in dir4
	@ ${CREATE_PKG} P7 spec  -in dir4

	@ ${TR} cmt "Detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer."
	@ ${TR} cmt "(see test_4.png)"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.4 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -I dir4 rules.1 > output.4
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end

clean:
	- ${RM} -rf output.* dir? dir?? *.~
	${TR} clean

