all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/Obj/testrec

.PHONY : clean

check: ../../Obj/archicheck
	${TR} create "Dependency list test"

	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B body -in dir1
	@ ${CREATE_PKG} C spec -in dir1 -with B -with F

	@ > dir1/c-d.ads
	@ echo "with E. G.H ;"	>> dir1/c-d.ads
	@ echo "package C .D is"	>> dir1/c-d.ads
	@ echo "   null;"		>> dir1/c-d.ads
	@ echo "end C . D ;"		>> dir1/c-d.ads

	@ ../../Obj/archicheck -I dir1 --list_dependencies | sort > dependency_list
	${TR} run "Dependency list test" true /usr/bin/sdiff "-s expected_dependency_list dependency_list"

	${TR} end

clean:
	- ${RM} -rf dependency_list dir1 
	${TR} clean

