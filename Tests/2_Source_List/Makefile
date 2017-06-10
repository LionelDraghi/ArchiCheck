all: check

.PHONY : clean

check: ../../Obj/archicheck clean
	@ echo source list test
	@ mkdir dir1
	@ mkdir dir2
	@ mkdir dir3
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
	@ sdiff -s expected_file_list file_list
	@ echo OK

clean:
	@ ${RM} -rf expected_file_list file_list dir[123]

