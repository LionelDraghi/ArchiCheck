all: check doc

.PHONY : clean

TR=../../Tools/testrec

check: ../../obj/archicheck header test1 test2 test3 

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Component definition rules test suite"
	@ ${TR} cmt "   Check the component list extraction from various rule's files"

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "One component list"

	@ ${TR} cmt "Running"
	@ ${TR} cmt "> archicheck --list_rules rules.1"
	@ ${TR} cmt 
	@ ${TR} cmt "with file rules.1 :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "should output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_component_list.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck --list_rules rules.1 > component_list.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_component_list.1 component_list.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "GUI component contains 3 other components, declared one by one on the rules file"

	@ ${TR} cmt "Running"
	@ ${TR} cmt "> archicheck -lr rules.2"
	@ ${TR} cmt 
	@ ${TR} cmt "with file rules.2 :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "should output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_component_list.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck -lr rules.2 > component_list.2
	@ ${TR} assert true /usr/bin/sdiff "-s expected_component_list.2 component_list.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "GUI component contains 3 other components, declared all in one line in the rules file"

	@ ${TR} cmt "Running"
	@ ${TR} cmt "> archicheck -lr rules.3"
	@ ${TR} cmt 
	@ ${TR} cmt "with file rules.3 :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.3 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "should output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_component_list.3 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/archicheck -lr rules.3 > component_list.3
	@ ${TR} assert true /usr/bin/sdiff "-s expected_component_list.3 component_list.3"

	@ ${TR} end

clean:
	@ - ${RM} -rf component_list.? 
	@ ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/component_list.md
