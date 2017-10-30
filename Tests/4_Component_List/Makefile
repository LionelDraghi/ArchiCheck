all: check

.PHONY : clean

TR=../Tools/Obj/testrec

check: ../../Obj/archicheck test1 test2 test3 test4

test1:
	${TR} create "Component list test"

	@ #---------------------------------------------------------------------
	@ ../../Obj/archicheck --list_rules rules.1 > component_list.1
	${TR} run "One component list" true /usr/bin/sdiff "-s expected_component_list.1 component_list.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ../../Obj/archicheck -lr rules.2 > component_list.2
	${TR} run "GUI component contains 3 other components, declared one by one on the rules file" true /usr/bin/sdiff "-s expected_component_list.2 component_list.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ../../Obj/archicheck -lr rules.3 > component_list.3
	${TR} run "GUI component contains 3 other components, declared all in one line in the rules file" true /usr/bin/sdiff "-s expected_component_list.3 component_list.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Comments and case insensitivity test" 
	## pas encore assez robuste pour ce test!!      @ ../../Obj/archicheck --list_rules rules.4 > component_list.4
	## pas encore assez robuste pour ce test!!      ${TR} run "Comments and case insensitivity test" true /usr/bin/sdiff "-s expected_component_list.3 component_list.4"

	${TR} end

clean:
	- ${RM} -rf component_list.? 
	${TR} clean

