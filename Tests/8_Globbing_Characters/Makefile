all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck test1 test2

test1:
	${TR} create "Globbing Character suite"

	${TR} start "rules test"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir1 -with Interfaces.C.Strings

	@ > rules.1
	@ echo "Application_Layer contains P1, P2"		>> rules.1
	@ echo "Support_Layer contains P3, P4"			>> rules.1
	@ echo "only Support_Layer may use Interfaces.*"	>> rules.1

	@ > expected_output.1
	## pas encore assez robuste pour ce test!! @ ../../Obj/archicheck -I dir1 rules.1 > output.1
	## pas encore assez robuste pour ce test!! ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "illegal use of Interfaces from Application_Layer"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2 -with Interfaces.C
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir1 -with Interfaces.C.Strings

	@ > rules.2
	@ echo "Application_Layer contains P1, P2"		>> rules.2
	@ echo "Support_Layer contains P3, P4"			>> rules.2
	@ echo "only Support_Layer may use Interfaces.*"	>> rules.2

	@ echo "Error : P4 is not in Support_Layer layer, and so shall not use Interfaces.* packages" > expected_output.2
	## pas encore assez robuste pour ce test!! @ ../../Obj/archicheck -I dir2 rules.2 > output.2
	## pas encore assez robuste pour ce test!! ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

clean:
	- ${RM} -rf expected_output.* output.* rules.? dir? dir?? *.~
	${TR} clean

