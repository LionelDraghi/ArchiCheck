all: file_list dependency_list return_code

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

	@ > expected_file_list
	@ echo "${PWD}/dir1/a.ads"	>> expected_file_list
	@ echo "${PWD}/dir1/a.adb"	>> expected_file_list
	@ echo "${PWD}/dir1/b.ads"	>> expected_file_list
	@ echo "${PWD}/dir1/c.ads"	>> expected_file_list

	@ archicheck -I dir1 --list_files > file_list
	@ diff expected_file_list file_list
	@ echo OK

dependency_list: archicheck
	@ echo dependency list test
	@ echo "with B;"	>> dir1/a.ads
	@ echo "package A is"	>> dir1/a.ads
	@ echo "   null;"	>> dir1/a.ads
	@ echo "end A;"		>> dir1/a.ads

	@ echo "with C;"		>> dir1/a.adb
	@ echo "package body A is"	>> dir1/a.adb
	@ echo "   null;"		>> dir1/a.adb
	@ echo "end A;"			>> dir1/a.adb

	@ echo "package B is"	>> dir1/b.ads
	@ echo "   null;"	>> dir1/b.ads
	@ echo "end B;"		>> dir1/b.ads

	@ echo "with B;"	>> dir1/c.ads
	@ echo "package C is"	>> dir1/c.ads
	@ echo "   null;"	>> dir1/c.ads
	@ echo "end C;"		>> dir1/c.ads

	@ > expected_dependency_list
	@ echo "A specification depends on B"	>> expected_dependency_list
	@ echo "A body          depends on C"	>> expected_dependency_list
	@ echo "C specification depends on B"	>> expected_dependency_list

	@ archicheck -I dir1 --list_dependencies > dependency_list
	@ diff expected_dependency_list dependency_list
	@ echo OK

.PHONY : clean
clean:
	${RM} -rf expected_file_list file_list \
              expected_dependency_list dependency_list \
              dir1/*
