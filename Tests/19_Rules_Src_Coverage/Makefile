all: check doc

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

.PHONY : clean

check: ../../obj/archicheck header test1 test2 test3

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Rules vs sources coverage test suite"

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Warnings on units appearing in rules file and not related to any source"

	@ ${CREATE_PKG} P1 -in dir1
	@ ${CREATE_PKG} P2 -in dir1
	@ ${CREATE_PKG} P5 -in dir1

	@ ${TR} cmt 
	@ ${TR} cmt "Sources (`ls -1 dir1`) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ls -1 dir1	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "Rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat test1.ac	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When :"
	@ ${TR} cmt 
	@ ${TR} cmt "> archicheck test1.ac -I ./dir1"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.1	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck test1.ac -I ./dir1 > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Non covered sources"

	@ ${CREATE_PKG} P2   -in dir2
	@ ${CREATE_PKG} P3   -in dir2
	@ ${CREATE_PKG} P4   -in dir2
	@ ${CREATE_PKG} P5   -in dir2
	@ ${CREATE_PKG} P1.X -in dir2
	@ ${CREATE_PKG} Y.P1 -in dir2
	@ ${CREATE_PKG} Framework.Utilities -in dir2
	@ ${CREATE_PKG} Framework_Utilities -in dir2
	@ ${CREATE_PKG} Java.Awt            -in dir2
	@ ${CREATE_PKG} Java                -in dir2

	@ ${TR} cmt "Rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat test1.ac	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "Sources (`ls -1 dir2`) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ls -1 dir2	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When :"
	@ ${TR} cmt 
	@ ${TR} cmt "> archicheck -lnc test2.ac -I ./dir2"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.2	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck -lnc test2.ac -I ./dir2 | sort > output.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"


	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Case insensitivity of Is_A_Component function (non reg)"

	@ ${CREATE_PKG} P1 -in dir3
	@ # useless package creation just to avoid warning

	@ ${TR} cmt "Rules file :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat test3.ac	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When :"
	@ ${TR} cmt 
	@ ${TR} cmt "> archicheck test3.ac -I dir3"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.3	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck test3.ac -I dir3 > output.3
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* *.~ dir?
	@ - ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/rules_src_coverage.md

