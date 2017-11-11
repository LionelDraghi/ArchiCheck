all: check

.PHONY : clean

TR=../Tools/testrec

check: ../../Obj/archicheck header test1 test2 test3 test4 test5


header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Rules file syntax test suite"
	${TR} cmt ""								
	${TR} cmt "Those tests check that the variaton in comment, casing,"	
	${TR} cmt "punctuation, etc. do not impact rules understanding."		
	${TR} cmt ""		


test1:
	@ #---------------------------------------------------------------------
	${TR} start "Reference file"

	${TR} cmt "Test: Reference file"						
	${TR} cmt ""								
	${TR} cmt "This rules file will serve as reference for all folowing tests"	
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat reference_rules.txt >> testrec.txt						
	${TR} cmt "(end)"								
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	${TR} cmt "archicheck -lr reference_rules.txt"				
	${TR} cmt "(end)"								
	${TR} cmt ""								
	${TR} cmt "The reference result is :"					
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat reference_lc.txt	>> testrec.txt						
	${TR} cmt "(end)"								
	${TR} cmt ""								

	@ ../../Obj/archicheck -lr reference_rules.txt	> lc1.txt
	${TR} assert true /usr/bin/sdiff "-s lc1.txt reference_lc.txt" 

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "Casing"
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat rules2.txt >> testrec.txt							
	${TR} cmt "(end)"								
	${TR} cmt ""								

	# File name is part of the log, so the "sed" before comparing the output with the reference output
	@ cat reference_lc.txt | sed "s/reference_rules\.txt/rules2\.txt/" > expected_lc2.txt

	@ ../../Obj/archicheck -lr rules2.txt  > lc2.txt
	${TR} assert true /usr/bin/sdiff "-s lc2.txt expected_lc2.txt" 

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "Spacing and comments"	
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat rules3.txt	>> testrec.txt						
	${TR} cmt "(end)"								
	${TR} cmt ""								

	# File name is part of the log, so the "sed" before comparing the output with the reference output
	@ cat reference_lc.txt | sed "s/reference_rules\.txt/rules3\.txt/" > expected_lc3.txt

	@ ../../Obj/archicheck -lr rules3.txt	> lc3.txt
	@ #### pas encore mur pour ce test!!    ${TR} assert true /usr/bin/sdiff "-s lc3.txt expected_lc3.txt"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "NaturalDocs format"
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat rules4.txt	>> testrec.txt						
	${TR} cmt "(end)"								
	${TR} cmt ""								

	# File name is part of the log, so the "sed" before comparing the output with the reference output
	@ cat reference_lc.txt | sed "s/reference_rules\.txt/rules4\.txt/" > expected_lc4.txt

	@ ../../Obj/archicheck -lr rules4.txt	> lc4.txt
	@ #### pas encore mur pour ce test!!    ${TR} assert true /usr/bin/sdiff "-s lc4.txt expected_lc4.txt"

	${TR} end

test5:
	@ #---------------------------------------------------------------------
	${TR} start "Punctuation and syntaxic sugar"
	${TR} cmt "Rules using syntaxic sugar, such as comma, semicolon, and, dot" 
	${TR} cmt "Almost natural english written rules file!"                     
	${TR} cmt ""								
	${TR} cmt "(start code)"							
	@ cat rules5.txt	>> testrec.txt						
	${TR} cmt "(end)"								
	${TR} cmt ""								

	@ ../../Obj/archicheck -lr rules5.txt  > lc5.txt
	@ #### pas encore mur pour ce test (bug du blanc comme spéparateur) !!   	${TR} assert true /usr/bin/sdiff "-s lc5.txt expected_lc5.txt"

	${TR} end

clean:
	- ${RM} -rf lc?.txt expected_lc?.txt
	${TR} clean

