all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3

.PHONY : header
header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "ArchiCheck code test suite"
	@ - ln -s ../../Src/ ./src

test1:
	@ #---------------------------------------------------------------------
	${TR} start "-lf test"

	ls /home/lionel/Proj/Archicheck/Src/*.ad? | sort > expected_output.1

	CMD="archicheck -lf -I src "


	@ ${TR} cmt 'Executed:'
	@ ${TR} cmt '`$CMD`'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 3 expected_output.1 >> testrec.md
	echo '...'					>> testrec.md
	tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	../../Obj/archicheck -lf -I src | sort > output.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "-ld test"

	@ ${TR} cmt "> archicheck -ld -I ./src | sort"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 10 expected_output.2 >> testrec.md
	echo '...'				  	 >> testrec.md
	tail -n 10 expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	../../Obj/archicheck -ld -I ./src | sort > output.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "rules test"

	@ ${TR} cmt "> archicheck archicheck.ac -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat archicheck.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	> expected_output.3
	../../Obj/archicheck archicheck.ac -I ./src > output.3
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	${TR} end

clean:
	- ${RM} -rf output.* src
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/archicheck.md

