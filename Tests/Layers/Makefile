all: check

CREATE_PKG=../Tools/create_pkg

.PHONY : clean

check: ./archicheck

	@ echo rules test
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1
	@ ${CREATE_PKG} P4 spec  -in dir1

	@ > rules.1
	@ echo "Layer_A contains P1, P2"		>> rules.1
	@ echo "Layer_B contains P3, P4"		>> rules.1
	@ echo "Layer_A is a layer over Layer_B"	>> rules.1

	@ > expected_output.1
	@ ./archicheck -I dir1 rules.1 > output.1
	@ diff -B expected_output.1 output.1
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo reverse dependency test
	@ ${CREATE_PKG} P1 spec  -in dir2 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir2 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir2
	@ ${CREATE_PKG} P4 spec  -in dir2 -with P5
	@ ${CREATE_PKG} P5 spec  -in dir2

	@ > rules.2
	@ echo "Layer_A contains P1, P2, P5"		>> rules.2
	@ echo "Layer_B contains P3, P4"		>> rules.2
	@ echo "Layer_A is a layer over Layer_B"	>> rules.2

	@ echo "Error : P4 is in Layer_B layer, and so shall not use the upper Layer_A layer" > expected_output.2
	@ ./archicheck -I dir2 rules.2 > output.2
	@ diff -B expected_output.2 output.2
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo layer bridging test
	@ ${CREATE_PKG} P1 spec  -in dir3a -with P2
	@ ${CREATE_PKG} P2 spec  -in dir3a -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir3b
	@ ${CREATE_PKG} P4 spec  -in dir3b
	@ ${CREATE_PKG} P6 spec  -in dir3c -with P4

	@ echo "Error : P6 is not in Layer_A layer, and so shall not directly use Layer_B layer" > expected_output.3
	@ ./archicheck -I dir3a -I dir3b -I dir3c rules.1 > output.3
	@ diff -B expected_output.3 output.3
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo undescribed dependency test
	@ ${CREATE_PKG} P1 spec  -in dir4 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir4 -with P3 -with P4 -with P7
	@ ${CREATE_PKG} P3 spec  -in dir4
	@ ${CREATE_PKG} P4 spec  -in dir4
	@ ${CREATE_PKG} P7 spec  -in dir4

	@ echo "Warning : P2 (in Layer_A layer) uses P7 that is neither in the same layer, nor in the lower Layer_B layer" > expected_output.4
	@ ./archicheck -I dir1 -I dir4 rules.1 > output.4
	@ diff -B expected_output.4 output.4
	@ echo OK

clean:
	@ ${RM} -rf expected_output.* output.* rules.? dir? dir?? *.dia~

