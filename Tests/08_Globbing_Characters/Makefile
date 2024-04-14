all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=globbing_characters.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2

header:
	@ ${TR} clean
	@ ${TR} create "Globbing Character test suite"

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "rules test"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir1 -with Interfaces.C.Strings

	@ > rules.1
	@ echo "Application_Layer contains P1, P2"		>> rules.1
	@ echo "Support_Layer contains P3, P4"			>> rules.1
	@ echo "only Support_Layer may use Interfaces*"	>> rules.1

	@ ${TR} cmt
	@ ${TR} cmt "Testing this dependencies :"
	@ ${TR} cmt
	@ ${TR} cmt "![](gc1.png)"
	@ ${TR} cmt
	@ ${TR} cmt "against this rules file :"
	@ ${TR} cmt
	@ ${TR} cmt '```'
	@ cat rules.1 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt
	@ ${TR} cmt "No expected output (no error)"

	@ > expected_output.1
	@ ../../obj/acc -I dir1 rules.1 > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "illegal use of Interfaces from Application_Layer"
	@ ${CREATE_PKG} P1 spec  -in dir2 -with P2 -with Interfaces.C
	@ ${CREATE_PKG} P2 spec  -in dir2 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir2 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir2 -with Interfaces.C.Strings

	@ > rules.2
	@ echo "Application_Layer contains P1, P2"		>> rules.2
	@ echo "Support_Layer contains P3, P4"		>> rules.2
	@ echo "only Support_Layer may use Interfaces*"	>> rules.2

	@ echo "Error : dir2/p1.ads:2: Only Support_Layer is allowed to use Interfaces*, P1 is not" > expected_output.2

	@ ${TR} cmt
	@ ${TR} cmt "Testing this dependencies :"
	@ ${TR} cmt
	@ ${TR} cmt "![](gc2.png)"
	@ ${TR} cmt
	@ ${TR} cmt "against this rules file :"
	@ ${TR} cmt
	@ ${TR} cmt '```'
	@ cat rules.2 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> ${MD}
	@ ${TR} cmt '```'

	@ test ! `../../obj/acc -I dir2 rules.2 > output.2`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

clean:
	@ - ${RM} -rf expected_output.* output.* rules.? dir? dir?? *.~
	@ ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
	@ cp gc?.png	../../docs/tests/
