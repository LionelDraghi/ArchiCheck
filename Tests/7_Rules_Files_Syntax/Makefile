all: check

.PHONY : clean

TR=../Tools/Obj/testrec

check: ../../Obj/archicheck test1 test2 test3 test4 test5


test1:
	@ echo Rules file syntax test

	@ echo "File: Rules file syntax"					>  Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "Those tests check that the variaton in comment, casing,"	>> Readme.txt
	@ echo "punctuation, etc. do not impact rules understanding."		>> Readme.txt
	@ echo ""		>> Readme.txt

	@ #---------------------------------------------------------------------
	@ echo "Test: Reference file"						>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "This rules file will serve as reference for all folowing tests"	>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat reference_rules.txt						>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ echo "archicheck -lr reference_rules.txt"				>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "The reference result is :"					>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat reference_lc.txt							>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt

	@ ../../Obj/archicheck -lr reference_rules.txt	> lc1.txt
	@ sdiff -s lc1.txt reference_lc.txt 

test2:
	@ #---------------------------------------------------------------------
	@ echo "Test: Casing"							>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat rules2.txt							>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt

	@ ../../Obj/archicheck -lr rules2.txt	> lc2.txt
	@ sdiff -s lc2.txt reference_lc.txt 

test3:
	@ #---------------------------------------------------------------------
	@ echo "Test: Spacing and comments"					>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat rules3.txt							>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt

	@ ../../Obj/archicheck -lr rules3.txt	> lc3.txt
	@ sdiff -s lc3.txt reference_lc.txt 

test4:
	@ #---------------------------------------------------------------------
	@ echo "Test: NaturalDocs format"					>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat rules4.txt							>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt

	@ ../../Obj/archicheck -lr rules4.txt	> lc4.txt
	@ sdiff -s lc4.txt reference_lc.txt 

test5:
	@ #---------------------------------------------------------------------
	@ echo "Test: Punctuation and syntaxic sugar"				>> Readme.txt
	@ echo ""								>> Readme.txt
	@ echo "(start code)"							>> Readme.txt
	@ cat rules5.txt							>> Readme.txt
	@ echo "(end)"								>> Readme.txt
	@ echo ""								>> Readme.txt

	@ ../../Obj/archicheck -lr rules5.txt	> lc5.txt
	@ sdiff -s lc5.txt reference_lc.txt 


	@ echo OK

clean:
	- ${RM} -rf lc?.txt 
	${TR} clean

