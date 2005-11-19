all: return_code file_list dependency_list

.PHONY : clean return_code file_list dependency_list

return_code: archicheck
	@ echo archicheck return code test
	@ test ! `archicheck -I		>/dev/null 2>&1`
	@ test ! `archicheck -I qsdqjh	>/dev/null 2>&1`
	@ archicheck -I dir1		>/dev/null 2>&1
	@ echo OK

file_list: archicheck
	@ echo source list test
	@ > dir1/a.ads
	@ > dir1/a.adb
	@ > dir1/b.ads
	@ > dir1/c.ads
	@ > dir1/c-d.ads

	@ > expected_file_list
	@ echo "${PWD}/dir1/a.ads"	>> expected_file_list
	@ echo "${PWD}/dir1/a.adb"	>> expected_file_list
	@ echo "${PWD}/dir1/b.ads"	>> expected_file_list
	@ echo "${PWD}/dir1/c.ads"	>> expected_file_list
	@ echo "${PWD}/dir1/c-d.ads"	>> expected_file_list

	@ archicheck -I dir1 --list_files > file_list
	@ diff expected_file_list file_list
	@ echo OK

dependency_list: archicheck
	@ echo dependency list test
	@ > dir2/a.ads
	@ echo "with B;"	>> dir2/a.ads
	@ echo "package A is"	>> dir2/a.ads
	@ echo "   null;"	>> dir2/a.ads
	@ echo "end A;"		>> dir2/a.ads

	@ > dir2/a.adb
	@ echo "with C;"		>> dir2/a.adb
	@ echo "package body A is"	>> dir2/a.adb
	@ echo "   null;"		>> dir2/a.adb
	@ echo "end A;"			>> dir2/a.adb

	@ > dir2/b.ads
	@ echo "package B is"	>> dir2/b.ads
	@ echo "   null;"	>> dir2/b.ads
	@ echo "end B;"		>> dir2/b.ads

	@ > dir2/c.ads
	@ echo "with B;"	>> dir2/c.ads
	@ echo "with F;"	>> dir2/c.ads
	@ echo "package C is"	>> dir2/c.ads
	@ echo "   null;"	>> dir2/c.ads
	@ echo "end C;"		>> dir2/c.ads

	@ > dir2/c-d.ads
	@ echo "with E. G.H ;"	>> dir2/c-d.ads
	@ echo "package C.D is"	>> dir2/c-d.ads
	@ echo "   null;"	>> dir2/c-d.ads
	@ echo "end C.D;"	>> dir2/c-d.ads

	@ > expected_dependency_list
	@ echo "A specification depends on B"		>> expected_dependency_list
	@ echo "A body          depends on C"		>> expected_dependency_list
	@ echo "C specification depends on B"		>> expected_dependency_list
	@ echo "C specification depends on F"		>> expected_dependency_list
	@ echo "C.D specification depends on E.G.H"	>> expected_dependency_list

	@ archicheck -I dir2 --list_dependencies > dependency_list
	@ diff expected_dependency_list dependency_list
	@ echo OK

clean:
	${RM} -rf expected_file_list file_list \
              expected_dependency_list dependency_list \
              dir1/*
