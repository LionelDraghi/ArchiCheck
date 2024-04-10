all: check doc

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

.PHONY : clean

check: ../../obj/acc header test1 

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Ada units test suite"
	@ ${CREATE_PKG} Sub spec -in src 
	@ ${CREATE_PKG} A1  spec -in src 
	@ ${CREATE_PKG} A2  spec -in src 
	@ ${CREATE_PKG} A3  spec -in src 
	@ ${CREATE_PKG} A4  spec -in src 
	@ ${CREATE_PKG} A5  spec -in src 
	@ ${CREATE_PKG} A6  spec -in src 
	@ ${CREATE_PKG} A7  spec -in src 
	@ gnat make -q sub-test -Isrc

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Ada compilation units unit test"

	@ ${TR} cmt "procedure Sub.Test has several separate units :"
	@ ${TR} cmt 
	@ ${TR} cmt "- procedure renaming"
	@ ${TR} cmt "- generic package renaming"
	@ ${TR} cmt 
	@ ${TR} cmt "- package body"
	@ ${TR} cmt "- package specification"
	@ ${TR} cmt "- generic package"
	@ ${TR} cmt 
	@ ${TR} cmt "- child procedure"
	@ ${TR} cmt 
	@ ${TR} cmt "- separate procedure"
	@ ${TR} cmt "- separate private procedure"
	@ ${TR} cmt "- separate package"
	@ ${TR} cmt "- separate function"
	@ ${TR} cmt "- separate task"
	@ ${TR} cmt "- separate protected"


	@ ${TR} cmt 
	@ ${TR} cmt "> acc -ld -I src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I src | sort > output.1
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end


clean:
	@ - ${RM} -rf output.* *.~ zip-ada 
	@ gnat clean -q sub-test
	@ - ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/ada_units.md

