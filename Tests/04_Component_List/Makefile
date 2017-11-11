all: check

.PHONY : clean

TR=../Tools/testrec

check: ../../Obj/archicheck header test1 test2 test3 test4 test5	

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "--list_rules option test suite"
	${TR} cmt "   Check the component list extraction from various rule's files"

test1:
	@ #---------------------------------------------------------------------
	${TR} start "One component list"

	${TR} cmt "Running"
	${TR} cmt "> archicheck --list_rules rules.1"
	${TR} cmt "with file rules.1 :"
	${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	${TR} cmt "(end)"
	${TR} cmt "should output :"
	${TR} cmt "(start code)"
	cat expected_component_list.1 >> testrec.txt
	${TR} cmt "(end)"

	@ ../../Obj/archicheck --list_rules rules.1 > component_list.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_component_list.1 component_list.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "GUI component contains 3 other components, declared one by one on the rules file"

	${TR} cmt "Running"
	${TR} cmt "> archicheck --list_rules rules.2"
	${TR} cmt "with file rules.2 :"
	${TR} cmt "(start code)"
	cat rules.2 >> testrec.txt
	${TR} cmt "(end)"
	${TR} cmt "should output :"
	${TR} cmt "(start code)"
	cat expected_component_list.2 >> testrec.txt
	${TR} cmt "(end)"

	@ ../../Obj/archicheck -lr rules.2 > component_list.2
	${TR} assert true /usr/bin/sdiff "-s expected_component_list.2 component_list.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "GUI component contains 3 other components, declared all in one line in the rules file"

	${TR} cmt "Running"
	${TR} cmt "> archicheck --list_rules rules.3"
	${TR} cmt "with file rules.3 :"
	${TR} cmt "(start code)"
	cat rules.3 >> testrec.txt
	${TR} cmt "(end)"
	${TR} cmt "should output :"
	${TR} cmt "(start code)"
	cat expected_component_list.3 >> testrec.txt
	${TR} cmt "(end)"

	@ ../../Obj/archicheck -lr rules.3 > component_list.3
	${TR} assert true /usr/bin/sdiff "-s expected_component_list.3 component_list.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Comments and case insensitivity test" 
	## pas encore assez robuste pour ce test!!      @ ../../Obj/archicheck --list_rules rules.4 > component_list.4
	## pas encore assez robuste pour ce test!!      ${TR} run "Comments and case insensitivity test" true /usr/bin/sdiff "-s expected_component_list.3 component_list.4"

	${TR} end

test5:
	@ #---------------------------------------------------------------------
	${TR} start "Almost natural english rules!" 
	## pas encore assez robuste pour ce test!!	@ ../../Obj/archicheck -lr rules.5 > component_list.5
	## pas encore assez robuste pour ce test!!	${TR} run "Rules using syntaxic sugar, such as comma, semicolon, and, dot" true /usr/bin/sdiff "-s expected_component_list.5 component_list.5"

	${TR} end

clean:
	- ${RM} -rf component_list.? 
	${TR} clean

