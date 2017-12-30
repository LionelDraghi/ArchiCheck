all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "ZipAda code test suite"

	@ # Creating the zipada dir :
	@ unzip -q -o zipada53.zip

test1:
	@ #---------------------------------------------------------------------
	${TR} start "-lf test"

	@ ${TR} cmt "> archicheck -lf -r -I ./zip-ada"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -lf -r -I ./zip-ada > output.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "-ld test"

	@ ${TR} cmt "> archicheck -ld -r -I ./zip-ada"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -r -I ./zip-ada | sort > output.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	${TR} end

clean:
	- ${RM} -rf output.* dir? dir?? *.~ zip-ada 
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/zipada.md

