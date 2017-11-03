all: check

.PHONY : clean

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

check: ../../Obj/archicheck 
	${TR} create "Sources list suite"

	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B spec -in dir2 -with K
	@ ${CREATE_PKG} C spec -in dir3 -with A
	@ ${CREATE_PKG} C.D spec -in dir3 -with B

	> expected_file_list
	@ echo $(CURDIR)"/dir1/a.adb" 		>> expected_file_list
	@ echo $(CURDIR)"/dir1/a.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir2/b.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c-d.ads" 	>> expected_file_list
	
	@ ../../Obj/archicheck -I dir1 -I dir2 -I dir3 --list_files | sort > file_list
	${TR} run "Source list test" true /usr/bin/sdiff "-s expected_file_list file_list"


clean:
	- ${RM} -rf expected_file_list file_list dir[123]
	${TR} clean

