all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4 test5 test6 test7

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Child packages test suite"

	@ echo "GUI is a layer over DB"	> rules.txt

	${TR} cmt "The tests hereafter are similar to those in [Layer rules tests suite], except that child packages allows to simplify rules file."
	${TR} cmt 
	${TR} cmt "The file rules :"
	@ ${TR} cmt 
	${TR} cmt '```'
	${TR} cmt "GUI contains P1 and P2"
	${TR} cmt "DB  contains P3 and P4"
	${TR} cmt "GUI is a layer over DB"
	${TR} cmt '```'
	${TR} cmt 
	${TR} cmt "is simplified in :"
	@ ${TR} cmt 
	${TR} cmt '```'
	@ cat rules.txt			>> testrec.md						
	${TR} cmt '```'
	@ ${TR} cmt 
	${TR} cmt "and packages GUI.P1, GUI.P2, DB.P3 and DB.P4 are created."


test1:
	@ #---------------------------------------------------------------------
	${TR} start "Rules OK test, no output expected"
	@ ${CREATE_PKG} GuI.P1 spec  -in dir1 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir1 -with DB.p3 -with dB.P4
	@ ${CREATE_PKG} Db.P3  spec  -in dir1
	@ ${CREATE_PKG} DB.P4  spec  -in dir1

	@ ${TR} cmt "![](cp1.png)"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat rules.txt >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ > expected_output.1
	@ ../../Obj/archicheck -q -I dir1 rules.txt > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end


test2:
	@ #---------------------------------------------------------------------
	${TR} start "Reverse dependency test"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir2 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir2 -with DB.P3 -with DB.P4
	@ ${CREATE_PKG} DB.P3  spec  -in dir2
	@ ${CREATE_PKG} DB.P4  spec  -in dir2
	@ ${CREATE_PKG} DB.P4  body  -in dir2 -with GUI.P5
	@ ${CREATE_PKG} GUI.P5 spec  -in dir2

	@ ${TR} cmt "Detection of a dependancy from a lower layer component to an upper layer component."
	@ ${TR} cmt "![](cp2.png)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ test ! `../../Obj/archicheck --quiet -I dir2 rules.txt > output.2`
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "Layer bridging test"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir3 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir3 -with DB.P3 -with DB.P4
	@ ${CREATE_PKG} DB.P3  spec  -in dir3
	@ ${CREATE_PKG} DB.P4  spec  -in dir3
	@ ${CREATE_PKG} P6     spec  -in dir3 -with DB.P4

	@ ${TR} cmt "Detection of a dependancy link crossing a layer."
	@ ${TR} cmt "![](cp3.png)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.3 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -I dir3 rules.txt > output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Undescribed dependency test"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir4 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir4 -with DB.P3 -with DB.P4 -with P7
	@ ${CREATE_PKG} DB.P3  spec  -in dir4
	@ ${CREATE_PKG} DB.P4  spec  -in dir4
	@ ${CREATE_PKG} P7     spec  -in dir4

	@ ${TR} cmt "Detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer."
	@ ${TR} cmt "![](cp4.png)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.4 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -I dir4 rules.txt > output.4
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end


test5:
	@ #---------------------------------------------------------------------
	${TR} start "Packages in the same layer may with themself"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir5
	@ ${CREATE_PKG} GUI.P2 spec  -in dir5 -with GUI.P1
	@ ${CREATE_PKG} DB.P3  spec  -in dir5
	@ ${CREATE_PKG} DB.P4  spec  -in dir5 -with DB.P3

	@ > expected_output.5
	@ ../../Obj/archicheck -I dir5 rules.txt 	> output.5
	${TR} assert true /usr/bin/sdiff "-s expected_output.5 output.5"

	${TR} end


test6:
	@ #---------------------------------------------------------------------
	${TR} start "GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir6 -with DB.P1
	@ ${CREATE_PKG} GUIP2  spec  -in dir6 -with DB.P1
	@ ${CREATE_PKG} DB.P1  spec  -in dir6
	@ ${CREATE_PKG} DBP2   spec  -in dir6 -with DB.P1

	@ ../../Obj/archicheck rules.txt -I dir6   > output.6
	${TR} assert true /usr/bin/sdiff "-s expected_output.6 output.6"

	${TR} end

test7:
	@ #---------------------------------------------------------------------
	${TR} start "Forbidding Interfaces but allowing Interfaces.C"
	@ ${CREATE_PKG} P1 spec  -in dir7 -with Interfaces
	@ ${CREATE_PKG} P2 spec  -in dir7 -with Interfaces.C
	@ ${CREATE_PKG} P3 spec  -in dir7 -with Interfaces.Java

	@ test ! `../../Obj/archicheck rules7.txt -I dir7   > output.7`
	${TR} assert true /usr/bin/sdiff "-s expected_output.7 output.7"

	${TR} end

clean:
	- ${RM} -rf output.* rules.txt dir? dir??  
	${TR} clean

doc:
	cp testrec.md 	../../docs/tests/child_packages.md
	cp cp?.png 	../../docs/tests/
