all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=files_component.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Adding compilation units through files to a Component"

test1: 
	@ #---------------------------------------------------------------------
	@ ${TR} start "Sources identification through rules file (no -I on command line)"
	@ ${TR} cmt "(src not detailed here)"
	@ ${TR} cmt "> acc -I dir1 --list_dependencies"
	@ ${TR} cmt "should put:"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.1 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt ""

	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B body -in dir2
	@ ${CREATE_PKG} C spec -in dir2 -with B -with F

	@ ../../obj/acc -I dir1 --list_dependencies | sort > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Source with weird formatted withed unit"


	@ ${CREATE_PKG} A spec -in dir2 -with B
	@ ${CREATE_PKG} A body -in dir2 -with C
	@ ${CREATE_PKG} B body -in dir2
	@ ${CREATE_PKG} C spec -in dir2 -with B -with F

	@ > dir2/c-d.adb
	@ echo "with   "			>> dir2/c-d.adb
	@ echo "     E, -- xyz"		>> dir2/c-d.adb
	@ echo "  F, G;"			>> dir2/c-d.adb
	@ echo "function C .D is"	>> dir2/c-d.adb
	@ echo "   null;"			>> dir2/c-d.adb
	@ echo "end C . D ;"		>> dir2/c-d.adb

	@ ${TR} cmt 'same test here, but `c-d.adb` if formatted this way :'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat dir2/c-d.adb 	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -I dir2 --list_dependencies | sort > output.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	@ ${TR} end


clean:
	- ${RM} -rf output.? dir?
	@ ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
