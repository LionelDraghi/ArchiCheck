all: create_pkg testrec check_suite check_test run_test check_output_file check_clean doc

TR=./testrec

create_pkg:
	gprbuild -P create_pkg.gpr

testrec:
	alr build --release

check_suite: ${TR}
	${TR} clean
	${TR} state > state.ref

	@ # Test Suite creation :
	${TR} cmt "Before any Suite or test creation, comments should start in column 0"
	${TR} create Suite_1
	${TR} cmt "After Create, comments should start in column 4"
	${TR} cmt "This is an empty suite..."
	${TR} state > state.1
	test ! `sdiff -sB state.ref state.1 > diff.1`
	sdiff -sB diff.1 expected.1

	@ # Another Test Suite creation :
	${TR} state > state.ref
	${TR} create Suite_2
	${TR} state > state.2
	test ! `sdiff -sB state.ref state.2 > diff.2`
	sdiff -sB diff.2 expected.2


check_test: ${TR}
	${TR} start "assert test"
	${TR} cmt "Simple assertion test..."
	${TR} state > state.3
	sdiff -sB state.3 expected.3

	${TR} state > state.ref
	echo x > file1
	echo y > file2
	${TR} assert false /usr/bin/diff "file1 file2" > /dev/null
	${TR} state > state.4
	test ! `sdiff -sB state.ref state.4 > diff.4`
	sdiff -sB diff.4 expected.4

	${TR} state > state.ref
	${TR} end
	${TR} state > state.5
	test ! `sdiff -sB state.ref state.5 > diff.5`
	sdiff -sB diff.5 expected.5

	${TR} start "another test"
	${TR} cmt "More assertions OK and NOK..."
	${TR} state > state.6
	sdiff -sB state.6 expected.6

	@ # Two assert OK
	${TR} state > state.ref
	${TR} assert false /usr/bin/diff "file1 file2" > /dev/null
	${TR} assert true  /usr/bin/diff "file1 file1"
	${TR} state > state.7
	test ! `sdiff -sB state.ref state.7 > diff.7`
	sdiff -sB diff.7 expected.7

	@ # One assert failed
	${TR} state > state.ref
	test ! `${TR} assert true /usr/bin/diff "file1 file2" > /dev/null`
	${TR} state > state.8
	test ! `sdiff -sB state.ref state.8 > diff.8`
	sdiff -sB diff.8 expected.8

	${TR} state > state.ref
	${TR} end
	${TR} state > state.9
	test ! `sdiff -sB state.ref state.9 > diff.9`
	sdiff -sB diff.9 expected.9

run_test: ${TR}
	${TR} clean
	${TR} create "run test suite"

	${TR} state > state.ref
	${TR} run "diff return false test" false /usr/bin/diff "file1 file2" > /dev/null
	${TR} state > state.10
	test ! `sdiff -sB state.ref state.10 > diff.10`
	sdiff -sB diff.10 expected.10

	${TR} state > state.ref
	${TR} run "diff return true test" true /usr/bin/diff "file1 file1" > /dev/null
	${TR} state > state.11
	test ! `sdiff -sB state.ref state.11 > diff.11`
	sdiff -sB diff.11 expected.11

check_output_file: ${TR}
	rm .testrec
	${TR} -o output.md state > state.ref

	${TR} -o output.md create Suite_X
	${TR} -o output.md create Suite_Y
	${TR} -o output.md start Test_Z
	${TR} -o output.md assert true  /bin/ls "-lt" > /dev/null
	${TR} -o output.md assert false cmd ""
	${TR} -o output.md end
	${TR} -o output.md state > state.12a
	sdiff -sB expected.12a state.12a

	${TR} -o output.md clean
	${TR} -o output.md state > state.12b
	sdiff -sB state.ref state.12b

check_clean: ${TR}
	rm .testrec
	${TR} state > state.ref

	${TR} create Suite_X
	${TR} create Suite_Y
	${TR} start Test_Z
	${TR} assert true  /bin/ls "-lt" > /dev/null
	${TR} assert false cmd ""
	${TR} end

	${TR} clean
	${TR} state > state.13
	sdiff -sB state.ref state.13

doc:
	# cp tools.md ../docs/tests/

.PHONY : clean
clean:
	- ${RM} testrec create_pkg obj/* .testrec output.md state.? state.?? testrec.md diff.? diff.?? file? state.ref

