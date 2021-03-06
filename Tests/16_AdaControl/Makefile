all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3

.PHONY : header
header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "AdaControl code test suite"

	@ # Creating the project dir :
	@ tar zxf adactl-1.19r10-src.tgz

test1:
	@ #---------------------------------------------------------------------
	${TR} start "-lf test"

	ls adactl-1.19r10/src/*.ad? | sort > expected_output.1

	@ ${TR} cmt "> archicheck -lf -r -I ./adactl-1.19r10/src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 3 expected_output.1 >> testrec.md
	echo '...'					>> testrec.md
	tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -lf -r -I ./adactl-1.19r10/src | sort > output.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "-ld test"

	@ ${TR} cmt "> archicheck -ld -r -I ./adactl-1.19r10/src | sort"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 10 expected_output.2 >> testrec.md
	echo '...'				  	 >> testrec.md
	tail -n 10 expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -r -I ./adactl-1.19r10/src | sort > output.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "rules test"

	@ ${TR} cmt "> archicheck adactl.ac -r -I ./adactl-1.19r10/src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat adactl.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	> expected_output.3
	@ ../../Obj/archicheck adactl.ac -r -I ./adactl-1.19r10/src > output.3
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	${TR} end

clean:
	- ${RM} -rf output.* dir? dir?? *.~ adactl-1.19r10 
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/adactl.md

