all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=use_rule.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2 test3 test4 test5 test6 test7 test8 test9

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Use rules test suite"


test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "May_Use rule, code compliant, no output expected"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3
	@ ${CREATE_PKG} P2 body  -in dir1 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1
	@ ${CREATE_PKG} P4 spec  -in dir1

	@ ${TR} cmt "Code compliant with the rules, should be OK"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.1 >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ > expected_output.1
	@ ../../obj/acc -q -I dir1 rules.1 > output.1
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Using a unit from a non allowed unit"
	@ ${CREATE_PKG} P1 spec  -in dir2 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir2 -with P3
	@ ${CREATE_PKG} P2 body  -in dir2 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir2
	@ ${CREATE_PKG} P4 spec  -in dir2
	@ ${CREATE_PKG} P1 body  -in dir2 -with Interfaces.C
	@ ${CREATE_PKG} P4 body  -in dir2 -with Interfaces.C
	
	@ ${TR} cmt "P4 body is using Interfaces.C : OK  "
	@ ${TR} cmt "P1 body is using Interfaces.C : should complain"
	@ ${TR} cmt 
	@ ${TR} cmt 'Expecting :'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ test ! `../../obj/acc -q -I dir2 rules.1 > output.2`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Forbidden use test"
	@ ${CREATE_PKG} P1 spec  -in dir3 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir3 -with P3
	@ ${CREATE_PKG} P2 body  -in dir3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir3
	@ ${CREATE_PKG} P4 spec  -in dir3

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.3 		>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'Expecting :'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.3	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ test ! `../../obj/acc -q -I dir3 rules.3 > output.3`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Allowing use of an environnement package"
	@ ${CREATE_PKG} P1 spec  -in dir4 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir4 -with Containers.Generic_Sort
	@ ${CREATE_PKG} P3 spec  -in dir4
	@ ${CREATE_PKG} P4 spec  -in dir4
	
	@ > rules.4
	@ echo "Layer_A contains P1 and P2" 		>> rules.4
	@ echo "Layer_B contains P3 and P4" 		>> rules.4
	@ echo "Layer_A is a layer over Layer_B"	>> rules.4
	@ ${TR} cmt 
	@ ${TR} cmt 'Rules :'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.4 	      	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 'Expecting :'
	@ ${TR} cmt 
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.4a	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ ${TR} cmt "First test without the allowing rule : should complain"
	@ test ! `../../obj/acc -I dir4 rules.4 > output.4a`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4a output.4a"

	@ ${TR} cmt "And now with the allowing rule :"
	@ echo "Containers use is allowed"	>> rules.4
	@ ${TR} cmt 
	@ ${TR} cmt 'Rules :'
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.4 	      	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt 'No error expected.'
	@ ${TR} cmt 

	@ > expected_output.4b
	@ ../../obj/acc -I dir4 rules.4 > output.4b
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4b output.4b"

	@ ${TR} end

test5:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Cumulative only ... may use X rules"
	@ ${CREATE_PKG} P1 spec  -in dir5 -with Interfaces.C
	@ ${CREATE_PKG} P2 spec  -in dir5 -with Interfaces.C
	@ ${CREATE_PKG} P3 spec  -in dir5 -with Interfaces.C
	@ ${CREATE_PKG} P4 spec  -in dir5 -with Interfaces.C

	@ ${TR} cmt "P1 P2 and P3 are withing Interfaces.C"
	@ ${TR} cmt 
	@ ${TR} cmt "rules5.txt contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.5 >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -I dir5 rules.5'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.5	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ test ! `../../obj/acc -I dir5 rules.5 > output.5`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.5 output.5"

	@ ${TR} end

	@ #---------------------------------------------------------------------
	@ ${TR} start "Unit_List may use ... rules"
	@ ${TR} cmt 
	@ ${TR} cmt "Same test with :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.5b >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "Same output expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.5	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ ${TR} cmt '** Not yet implemented **'

	@ ## Fixme: @ test ! `../../obj/acc -I dir5 rules.5b > output.5b`
	@ ## ${TR} assert true /usr/bin/sdiff "-sBW expected_output.5 output.5b"

	@ ${TR} end

test6:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Combining Allowed and Forbidden"
	@ ${CREATE_PKG} P1 spec  -in dir6 -with Interfaces
	@ ${CREATE_PKG} P2 spec  -in dir6 -with Interfaces.C
	@ ${CREATE_PKG} P3 spec  -in dir6 -with Interfaces.Java

	@ ${TR} cmt "P1 P2 and P3 are withing Interfaces, Interfaces.C and Interfaces.Java"
	@ ${TR} cmt 
	@ ${TR} cmt "rules6.txt contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.6 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -I dir6 rules.6'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"

	@ test ! `../../obj/acc -I dir6 rules.6 | sort > output.6`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.6 output.6"

	@ ${TR} cmt "And now inverting Allowed and Forbidden"
	@ ${TR} cmt 
	@ ${TR} cmt "rules6b.txt contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.6b >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -I dir6 rules.6b'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"

	@ ../../obj/acc -I dir6 rules.6b > output.6b
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.6b output.6b"

	@ ${TR} end

test7:
	@ #---------------------------------------------------------------------
	@ ${TR} start "X may use Unit List rules"
	@ ${CREATE_PKG} P1 spec -in dir7 -with P2 -with P3 -with P4
	@ ${CREATE_PKG} P2 spec -in dir7 -with P1
	@ ${CREATE_PKG} P3 spec -in dir7 -with P1
	@ ${CREATE_PKG} P4 spec -in dir7 -with P1

	@ ${TR} cmt "P1 withing P2, P3, P4"
	@ ${TR} cmt 
	@ ${TR} cmt "rules7.txt contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.7 >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -I dir7 rules.7'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.7	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ test ! `../../obj/acc -I dir7 rules.7 > output.7`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.7 output.7"

	@ ${TR} end

test8:
	@ #---------------------------------------------------------------------
	@ ${TR} start "only X may use Unit List rules"
	@ ${CREATE_PKG} P1 spec -in dir8 -with P2 -with P3 
	@ ${CREATE_PKG} P4 spec -in dir8 -with P2 -with P3

	@ ${TR} cmt "P1 withing P2 and P3"
	@ ${TR} cmt "P4 withing P2 and P3"
	@ ${TR} cmt 
	@ ${TR} cmt "rules8.txt contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.8 >> ${MD}
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -I dir8 rules.8'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.8	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ test ! `../../obj/acc -I dir8 rules.8 > output.8`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.8 output.8"

	@ ${TR} end

test9:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Appending rules"
	@ ${CREATE_PKG} P1  spec -in dir9 
	@ ${CREATE_PKG} P2  spec -in dir9 -with P2
	@ ${CREATE_PKG} P3  spec -in dir9 -with P2 -with P3 
	@ ${CREATE_PKG} P4  spec -in dir9 -with P2 -with P3
	@ ${CREATE_PKG} Bus spec -in dir9 
	@ ${CREATE_PKG} IO  spec -in dir9 
	@ > rules.9
	@ echo "Layer_A contains P1 and P2" 		>> rules.9
	@ echo "Layer_B contains P3 and P4" 		>> rules.9

	@ ${TR} cmt 
	@ ${TR} cmt "rules.9 contains :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat rules.9 >> testrec.md
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When running :"
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -lr -I dir9 -ar "only P1 may use IO" rules.9'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.9a	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.9c	>> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	
	@ test ! `../../obj/acc -lr -I dir9 -ar "only P1 may use IO" rules.9 > output.9a`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.9a output.9a"

	@ test ! `../../obj/acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9 > output.9c`
	@ ${TR} assert true /usr/bin/sdiff "-sBW expected_output.9c output.9c"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* dir? dir?? *.~
	@ ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
