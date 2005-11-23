all: check

.PHONY : clean

check: archicheck
	@ echo source list test
	@ > dir1/a.ads
	@ > dir1/a.adb
	@ > dir1/b.ads
	@ > dir1/c.ads
	@ > dir1/c-d.ads

	@ > expected_file_list
	@ echo "${PWD}/Source_List/dir1/a.ads"		>> expected_file_list
	@ echo "${PWD}/Source_List/dir1/a.adb"		>> expected_file_list
	@ echo "${PWD}/Source_List/dir1/b.ads"		>> expected_file_list
	@ echo "${PWD}/Source_List/dir1/c.ads"		>> expected_file_list
	@ echo "${PWD}/Source_List/dir1/c-d.ads"	>> expected_file_list

	@ archicheck -I dir1 --list_files > file_list
	@ diff expected_file_list file_list
	@ echo OK

clean:
	@ ${RM} -rf expected_file_list file_list dir1/*

