all: check

CREATE_PKG=../Tools/create_pkg

.PHONY : clean

check: ../../Obj/archicheck

	@ #---------------------------------------------------------------------
	@ echo rules OK test, no output expected
	@ ${CREATE_PKG} GuI.P1 spec  -in dir1 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir1 -with DB.p3 -with dB.P4
	@ ${CREATE_PKG} Db.P3  spec  -in dir1
	@ ${CREATE_PKG} DB.P4  spec  -in dir1

	@ > rules.1
	@ echo "GUi is a layer over Db"	>> rules.1

	@ > expected_output.1
	@ ../../Obj/archicheck -I dir1 rules.1 > output.1
	@ sdiff -s expected_output.1 output.1
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo reverse dependency test
	@ ${CREATE_PKG} GUI.P1 spec  -in dir2 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir2 -with DB.P3 -with DB.P4
	@ ${CREATE_PKG} DB.P3  spec  -in dir2
	@ ${CREATE_PKG} DB.P4  spec  -in dir2
	@ ${CREATE_PKG} DB.P4  body  -in dir2 -with GUI.P5
	@ ${CREATE_PKG} GUI.P5 spec  -in dir2

	@ > rules.2
	@ echo "GUI is a layer over DB"	>> rules.2

	@ echo "Error : DB.P4 is in DB layer, and so shall not use the upper GUI layer" > expected_output.2
	@ ../../Obj/archicheck -I dir2 rules.2 > output.2
	@ sdiff -s expected_output.2 output.2
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo layer bridging test
	@ ${CREATE_PKG} GUI.P1 spec  -in dir3 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir3 -with DB.P3 -with DB.P4
	@ ${CREATE_PKG} DB.P3  spec  -in dir3
	@ ${CREATE_PKG} DB.P4  spec  -in dir3
	@ ${CREATE_PKG} P6     spec  -in dir3 -with DB.P4

	@ echo "Error : P6 is not in GUI layer, and so shall not directly use DB layer" > expected_output.3
	@ ../../Obj/archicheck -I dir3 rules.2 > output.3
	@ sdiff -s expected_output.3 output.3
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo undescribed dependency test
	@ ${CREATE_PKG} GUI.P1 spec  -in dir4 -with GUI.P2
	@ ${CREATE_PKG} GUI.P2 spec  -in dir4 -with DB.P3 -with DB.P4 -with P7
	@ ${CREATE_PKG} DB.P3  spec  -in dir4
	@ ${CREATE_PKG} DB.P4  spec  -in dir4
	@ ${CREATE_PKG} P7     spec  -in dir4

	@ echo "Warning : GUI.P2 (in GUI layer) uses P7 that is neither in the same layer, nor in the lower DB layer" > expected_output.4
	@ ../../Obj/archicheck -I dir1 -I dir4 rules.2 > output.4
	@ sdiff -s expected_output.4 output.4
	@ echo OK

clean:
	@ ${RM} -rf expected_output.* output.* rules.? dir? dir??

