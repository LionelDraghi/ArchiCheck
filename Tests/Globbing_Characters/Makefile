all: check

CREATE_PKG=../Tools/create_pkg

.PHONY : clean

check: ../../Obj/archicheck

	@ echo rules test
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir1 -with Interfaces.C.Strings

	@ > rules.1
	@ echo "Application_Layer contains P1, P2"		>> rules.1
	@ echo "Support_Layer contains P3, P4"			>> rules.1
	@ echo "only Support_Layer may use Interfaces.*"	>> rules.1

	@ > expected_output.1
	@ ../../Obj/archicheck -I dir1 rules.1 > output.1
	@ sdiff -s expected_output.1 output.1
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo illegal use of Interfaces from Application_Layer
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2 -with Interfaces.C
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir1 -with Interfaces.C.Strings

	@ > rules.2
	@ echo "Application_Layer contains P1, P2"		>> rules.2
	@ echo "Support_Layer contains P3, P4"			>> rules.2
	@ echo "only Support_Layer may use Interfaces.*"	>> rules.2

	@ echo "Error : P4 is not in Support_Layer layer, and so shall not use Interfaces.* packages" > expected_output.2
	@ ../../Obj/archicheck -I dir2 rules.2 > output.2
	@ sdiff -s expected_output.2 output.2
	@ echo OK

clean:
	@ ${RM} -rf expected_output.* output.* rules.? dir? dir?? *.~

