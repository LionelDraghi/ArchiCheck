all: check doc

.PHONY : clean

check: ../../obj/acc header test1 test2

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "File identification test suite"

test1: ../../obj/acc 
	@ #---------------------------------------------------------------------
	@ ${TR} start "Non recursive file identification test"
	@ ${TR} cmt 
	@ ${TR} cmt "if sources :"
	@ ${TR} cmt "> ./dir3/c-d.ads  "
	@ ${TR} cmt "> ./dir3/c.ads  "
	@ ${TR} cmt "> ./dir1/a.ads  "
	@ ${TR} cmt "> ./dir1/a.adb  "
	@ ${TR} cmt "> ./dir2/b.ads  "
	@ ${TR} cmt 
	@ ${TR} cmt "then"
	@ ${TR} cmt 
	@ ${TR} cmt "> acc -I dir1 -I dir2 -I dir3 --list_files"
	@ ${TR} cmt 
	@ ${TR} cmt "should put:"

	@ ${TR} cmt '```'
	@ cat expected_file_list.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt ""

	@ ${CREATE_PKG} A spec -in dir1 -with B
	@ ${CREATE_PKG} A body -in dir1 -with C
	@ ${CREATE_PKG} B spec -in dir2 -with K
	@ ${CREATE_PKG} C spec -in dir3 -with A
	@ ${CREATE_PKG} C.D spec -in dir3 -with B

	@ ../../obj/acc -I dir1 -I dir2 -I dir3 --list_files | sort > file_list.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_file_list.1 file_list.1"

	@ ${TR} end

test2: ../../obj/acc 
	@ #---------------------------------------------------------------------
	@ ${TR} start "Mixt recursive and non-recursive file identification test"
	@ ${TR} cmt 
	@ ${TR} cmt "if sources :"
	@ ${TR} cmt "> ./dira/a.ads  "
	@ ${TR} cmt "> ./dira/dira1/c-d.ads  "
	@ ${TR} cmt "> ./dirb/b.ads  "
	@ ${TR} cmt "> ./dirb/dirb1/c.ads  "
	@ ${TR} cmt 
	@ ${TR} cmt "then"
	@ ${TR} cmt 
	@ ${TR} cmt "> acc -I dira -Ir dirb --list_files"
	@ ${TR} cmt 
	@ ${TR} cmt "should put:"

	@ ${TR} cmt '```'
	@ cat expected_file_list.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt ""

	@ ${CREATE_PKG} A spec   -in dira -with B
	@ ${CREATE_PKG} B spec   -in dirb -with K
	@ ${CREATE_PKG} C spec   -in dirb/dirb1 -with A
	@ ${CREATE_PKG} C.D spec -in dira/dira1 -with B

	@ ../../obj/acc -I dira -Ir dirb --list_files | sort > file_list.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_file_list.2 file_list.2"

	@ ${TR} end

clean:
	@ - ${RM} -rf file_list.? dir?
	@ ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/source_list.md
