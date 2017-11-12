all: check

.PHONY : clean

check: ../../Obj/archicheck header test1 

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "--list_files option test suite"

test1: ../../Obj/archicheck 
	@ #---------------------------------------------------------------------
	${TR} start "Simple test"
	${TR} cmt ""
	${TR} cmt "if sources :"
	${TR} cmt "> ./dir3/c-d.ads"
	${TR} cmt "> ./dir3/c.ads"
	${TR} cmt "> ./dir1/a.ads"
	${TR} cmt "> ./dir1/a.adb"
	${TR} cmt "> ./dir2/b.ads"
	${TR} cmt "then"
	${TR} cmt "> archicheck -I dir1 -I dir2 -I dir3 --list_files"
	${TR} cmt "should put:"

	> expected_file_list
	@ echo $(CURDIR)"/dir1/a.adb" 		>> expected_file_list
	@ echo $(CURDIR)"/dir1/a.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir2/b.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c-d.ads" 	>> expected_file_list

	${TR} cmt "(start code)"
	cat expected_file_list >> testrec.txt
	${TR} cmt "(end)"
	${TR} cmt ""

	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B spec -in dir2 -with K
	@ ${CREATE_PKG} C spec -in dir3 -with A
	@ ${CREATE_PKG} C.D spec -in dir3 -with B

	@ ../../Obj/archicheck -I dir1 -I dir2 -I dir3 --list_files | sort > file_list
	${TR} assert true /usr/bin/sdiff "-sbB expected_file_list file_list"

	${TR} end

clean:
	- ${RM} -rf expected_file_list file_list dir[123]
	${TR} clean

