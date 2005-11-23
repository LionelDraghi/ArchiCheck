all: check

.PHONY : clean

check: archicheck
	@ echo dependency list test
	@ > dir1/a.ads
	@ echo "with B;"	>> dir1/a.ads
	@ echo "package A is"	>> dir1/a.ads
	@ echo "   null;"	>> dir1/a.ads
	@ echo "end A;"		>> dir1/a.ads

	@ > dir1/a.adb
	@ echo "with C;"		>> dir1/a.adb
	@ echo "package body A is"	>> dir1/a.adb
	@ echo "   null;"		>> dir1/a.adb
	@ echo "end A;"			>> dir1/a.adb

	@ > dir1/b.ads
	@ echo "package B is"	>> dir1/b.ads
	@ echo "   null;"	>> dir1/b.ads
	@ echo "end B;"		>> dir1/b.ads

	@ > dir1/c.ads
	@ echo "with B;"	>> dir1/c.ads
	@ echo "with F;"	>> dir1/c.ads
	@ echo "package C is"	>> dir1/c.ads
	@ echo "   null;"	>> dir1/c.ads
	@ echo "end C;"		>> dir1/c.ads

	@ > dir1/c-d.ads
	@ echo "with E. G.H ;"	>> dir1/c-d.ads
	@ echo "package C.D is"	>> dir1/c-d.ads
	@ echo "   null;"	>> dir1/c-d.ads
	@ echo "end C.D;"	>> dir1/c-d.ads

	@ > expected_dependency_list
	@ echo "A specification depends on B"		>> expected_dependency_list
	@ echo "A body          depends on C"		>> expected_dependency_list
	@ echo "C specification depends on B"		>> expected_dependency_list
	@ echo "C specification depends on F"		>> expected_dependency_list
	@ echo "C.D specification depends on E.G.H"	>> expected_dependency_list

	@ archicheck -I dir1 --list_dependencies > dependency_list
	@ diff expected_dependency_list dependency_list
	@ echo OK

clean:
	@ ${RM} -rf expected_dependency_list dependency_list dir1/*

