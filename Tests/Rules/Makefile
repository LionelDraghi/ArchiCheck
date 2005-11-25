all: check

.PHONY : clean

check: archicheck
	@ echo rules test
	@ > dir1/p1.ads
	@ echo "with P2;"	>> dir1/p1.ads
	@ echo "package P1 is"	>> dir1/p1.ads
	@ echo "   null;"	>> dir1/p1.ads
	@ echo "end P1;"	>> dir1/p1.ads

	@ > dir1/p2.ads
	@ echo "with P3;"	>> dir1/p2.ads
	@ echo "with P4;"	>> dir1/p2.ads
	@ echo "package P2 is"	>> dir1/p2.ads
	@ echo "   null;"	>> dir1/p2.ads
	@ echo "end P2;"	>> dir1/p2.ads

	@ > dir1/p3.ads
	@ echo "package P3 is"	>> dir1/p3.ads
	@ echo "   null;"	>> dir1/p3.ads
	@ echo "end P3;"	>> dir1/p3.ads

	@ > dir1/p4.ads
	@ echo "package P4 is"	>> dir1/p4.ads
	@ echo "   null;"	>> dir1/p4.ads
	@ echo "end P4;"	>> dir1/p4.ads

	@ > rules.1
	@ echo "Layer_A contains P1, P2"		>> rules.1
	@ echo "Layer_B contains P3, P4"		>> rules.1
	@ echo "Layer_A is a layer over Layer_B"	>> rules.1

	@ > expected_output.1
	@ archicheck -I dir1 rules.1 > output.1
	@ diff -B expected_output.1 output.1
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo reverse dependency test
	@ > dir1/p5.ads
	@ echo "package P5 is"	>> dir1/p5.ads
	@ echo "   null;"	>> dir1/p5.ads
	@ echo "end P5;"	>> dir1/p5.ads

	@ > dir1/p4.ads
	@ echo "with P5;"	>> dir1/p4.ads
	@ echo "package P4 is"	>> dir1/p4.ads
	@ echo "   null;"	>> dir1/p4.ads
	@ echo "end P4;"	>> dir1/p4.ads

	@ > rules.2
	@ echo "Layer_A contains P1, P2, P5"		>> rules.2
	@ echo "Layer_B contains P3, P4"		>> rules.2
	@ echo "Layer_A is a layer over Layer_B"	>> rules.2

	@ echo "** P4 is in Layer_B layer, and so shall not use the upper Layer_A layer" > expected_output.2
	@ archicheck -I dir1 rules.2 > output.2
	@ diff -B expected_output.2 output.2
	@ echo OK

	@ #---------------------------------------------------------------------
	@ echo layer bridging test
	@ > dir2/p6.ads
	@ echo "with P4;"	>> dir2/p6.ads
	@ echo "package P6 is"	>> dir2/p6.ads
	@ echo "   null;"	>> dir2/p6.ads
	@ echo "end P6;"	>> dir2/p6.ads

	@ > dir1/p4.ads
	@ echo "package P4 is"	>> dir1/p4.ads
	@ echo "   null;"	>> dir1/p4.ads
	@ echo "end P4;"	>> dir1/p4.ads

	@ echo "** P6 is not in Layer_A layer, and so shall not directly use Layer_B layer" > expected_output.3
	@ archicheck -I dir1 -I dir2 rules.2 > output.3
	@ diff -B expected_output.3 output.3
	@ echo OK

clean:
	@ ${RM} -rf expected_dependency_list dependency_list dir1/*

