all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Ada units test suite"
	@ ${CREATE_PKG} Sub spec -in src 
	@ ${CREATE_PKG} A1  spec -in src 
	@ ${CREATE_PKG} A2  spec -in src 
	@ ${CREATE_PKG} A3  spec -in src 
	@ ${CREATE_PKG} A4  spec -in src 
	@ ${CREATE_PKG} A5  spec -in src 
	@ ${CREATE_PKG} A6  spec -in src 
	@ gnat make -q sub-test -Isrc

test1:
	@ #---------------------------------------------------------------------
	${TR} start "Subunits (separate) unit test"

	@ ${TR} cmt "procedure Sub.Test has several separate units :"
	@ ${TR} cmt "- separate procedure"
	@ ${TR} cmt "- separate package"
	@ ${TR} cmt "- separate function"
	@ ${TR} cmt "- separate task"
	@ ${TR} cmt "- separate protected"
	@ ${TR} cmt "> archicheck -ld -I ."
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.1 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -ld -I src > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end


clean:
	- ${RM} -rf output.* *.~ zip-ada 
	@gnat clean -q sub-test
	- ${TR} clean

