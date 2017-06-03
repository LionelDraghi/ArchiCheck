all: check

CREATE_PKG=../Tools/create_pkg

.PHONY : clean

check: ./archicheck
	@ echo dependency list test
	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B body -in dir1
	@ ${CREATE_PKG} C spec -in dir1 -with B -with F

	@ > dir1/c-d.ads
	@ echo "with E. G.H ;"		>> dir1/c-d.ads
	@ echo "package C .D is"	>> dir1/c-d.ads
	@ echo "   null;"		>> dir1/c-d.ads
	@ echo "end C . D ;"		>> dir1/c-d.ads

	@ ./archicheck -I dir1 --list_dependencies > dependency_list
	@ diff expected_dependency_list dependency_list
	@ echo OK

clean:
	@ ${RM} -rf dependency_list dir1

