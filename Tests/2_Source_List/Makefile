all: check

.PHONY : clean

check: ./archicheck clean
	@ echo source list test
	@ mkdir dir1
	@ mkdir dir2
	@ mkdir dir3
	@ > dir1/a.ads
	@ > dir1/a.adb
	@ > dir2/b.ads
	@ > dir3/c.ads
	@ > dir3/c-d.ads

	@ ./archicheck -I dir1 -I dir2 -I dir3 --list_files > file_list
	@ diff expected_file_list file_list
	@ echo OK

clean:
	@ ${RM} -rf file_list dir[123]

