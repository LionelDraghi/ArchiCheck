all: check

.PHONY : clean


check: ../../Obj/archicheck

	@ #---------------------------------------------------------------------
	@ echo component list test 1, one component
	@ ../../Obj/archicheck --list_components rules.1 > component_list.1
	@ diff expected_component_list.1 component_list.1
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo component list test 2, GUI component contains 3 other components
	@ ../../Obj/archicheck --list_components rules.2 > component_list.2
	@ diff expected_component_list.2 component_list.2
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo component list test 3, comments and case insensitivity test
	@ ../../Obj/archicheck --list_components rules.3 > component_list.3
	@ diff expected_component_list.3 component_list.3
	@ echo OK

clean:
	@ ${RM} -rf component_list.? 

