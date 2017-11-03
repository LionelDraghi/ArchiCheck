all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4 test5 test6

header:
	@ #---------------------------------------------------------------------
	${TR} create "Child packages suite"

	@ echo "GUI is a layer over DB"	> rules.txt

	${TR} cmt "Test in this suite are run against this simple rules file :"			
	${TR} cmt "(start code)"							
	@ cat rules.txt			>> testrec.txt						
	${TR} cmt "(end)"

test1:
	@ #---------------------------------------------------------------------
	${TR} start "Rules OK test, no output expected"
	@ ${CREATE_PKG} GuI.P1 spec  -in dir1 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir1 -with DB.p3 -with dB.P4
	@ ${CREATE_PKG} Db.P3  spec  -in dir1
	@ ${CREATE_PKG} DB.P4  spec  -in dir1

	@ > expected_output.1
	@ ../../Obj/archicheck -I dir1 rules.txt > output.1
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

	@ echo "Error : DB.P4 is in DB layer, and so shall not use GUI.P5 in the upper GUI layer" > expected_output.2
	@ ../../Obj/archicheck -I dir2 rules.txt > output.2
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

	@ echo "Warning : P6 is neither in GUI or DB layer, and so shall not directly use DB.P4 in the DB layer" > expected_output.3
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

	@ echo "Warning : GUI.P2 (in GUI layer) uses P7 that is neither in the same layer, nor in the lower DB layer" > expected_output.4

	@ ../../Obj/archicheck -I dir1 -I dir4 rules.txt 	> output.4
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
	${TR} start "GUI.P1 is a GUI child, GUIP1 is not"
	@ ${CREATE_PKG} GUI.P1 spec  -in dir6 -with DB.P1
	@ ${CREATE_PKG} GUIP2  spec  -in dir6 -with DB.P1
	@ ${CREATE_PKG} DB.P1  spec  -in dir6
	@ ${CREATE_PKG} DBP2   spec  -in dir6 -with DB.P1

	@ > expected_output.6
	@ echo "Warning : GUIP2 is neither in GUI or DB layer, and so shall not directly use DB.P1 in the DB layer" >> expected_output.6
	@ echo "Warning : DBP2 is neither in GUI or DB layer, and so shall not directly use DB.P1 in the DB layer"  >> expected_output.6

	@ ../../Obj/archicheck rules.txt -I dir6   > output.6
	${TR} assert true /usr/bin/sdiff "-s expected_output.6 output.6"

	${TR} end

clean:
	- ${RM} -rf expected_output.* output.* rules.txt dir? dir??  
	${TR} clean

