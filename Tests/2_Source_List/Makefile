all: check

.PHONY : clean

TR=../Tools/Obj/testrec

check: ../../Obj/archicheck 
	${TR} create "Sources list test"

	@ mkdir -p dir1
	@ mkdir -p dir2
	@ mkdir -p dir3
	@ > dir1/a.ads
	@ > dir1/a.adb
	@ > dir2/b.ads
	@ > dir3/c.ads
	@ > dir3/c-d.ads

	> expected_file_list
	@ echo $(CURDIR)"/dir1/a.adb" 		>> expected_file_list
	@ echo $(CURDIR)"/dir1/a.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir2/b.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c.ads" 		>> expected_file_list
	@ echo $(CURDIR)"/dir3/c-d.ads" 	>> expected_file_list
	
	@ ../../Obj/archicheck -I dir1 -I dir2 -I dir3 --list_files | sort > file_list
	${TR} run "Source list test" true /usr/bin/sdiff "-s expected_file_list file_list"


clean:
	${RM} -rf expected_file_list file_list dir[123]
	${TR} clean

