all: check doc

.PHONY : clean

TR=../../Tools/testrec

check: ../../obj/archicheck header test1 test2 test3 test4


header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Rules file syntax test suite"
	@ ${TR} cmt
	@ ${TR} cmt "Those tests check that the variaton in comment, casing,"	
	@ ${TR} cmt "punctuation, etc. do not impact rules understanding."		
	@ ${TR} cmt ""		


test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Reference file"

	@ ${TR} cmt "Test: Reference file"						
	@ ${TR} cmt
	@ ${TR} cmt '```'							
	@ cat expected_lc1.txt >> testrec.md						
	@ ${TR} cmt '```'								
	@ ${TR} cmt
	@ ${TR} cmt "> archicheck --list_rules expected_lc1.txt"				
	@ ${TR} cmt
	@ ${TR} cmt "The reference result is :"					
	@ ${TR} cmt
	@ ${TR} cmt '```'							
	@ cat reference_lc.txt	>> testrec.md						
	@ ${TR} cmt '```'								
	@ ${TR} cmt

	@ ../../obj/archicheck --list_rules rules1.txt	> lc1.txt
	@ ${TR} assert true /usr/bin/sdiff "-s lc1.txt expected_lc1.txt" 

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Casing"
	@ ${TR} cmt
	@ ${TR} cmt '```'							
	@ cat rules2.txt >> testrec.md							
	@ ${TR} cmt '```'								
	@ ${TR} cmt

	@ ../../obj/archicheck -lr rules2.txt  > lc2.txt
	@ ${TR} assert true /usr/bin/sdiff "-s lc2.txt expected_lc2.txt" 

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Spacing and comments"	
	@ ${TR} cmt
	@ ${TR} cmt '```'							
	@ cat rules3.txt	>> testrec.md						
	@ ${TR} cmt '```'								
	@ ${TR} cmt

	@ ../../obj/archicheck -lr rules3.txt	> lc3.txt
	@ ${TR} assert true /usr/bin/sdiff "-s lc3.txt expected_lc3.txt"
	
	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Punctuation and syntaxic sugar"
	@ ${TR} cmt "Rules using syntaxic sugar, such as comma, semicolon, and, dot" 
	@ ${TR} cmt "Almost natural english written rules file!"                     
	@ ${TR} cmt
	@ ${TR} cmt '```'							
	@ cat rules4.txt	>> testrec.md						
	@ ${TR} cmt '```'								
	@ ${TR} cmt

	@ #### ../../obj/archicheck -lr rules4.txt  > lc4.txt
	@ #### pas encore mur pour ce test (bug du blanc comme spéparateur) !!	@ ${TR} assert true /usr/bin/sdiff "-s lc4.txt expected_lc4.txt"

	@ ${TR} end

clean:
	@ - ${RM} -rf lc?.txt 
	@ ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/rules_files_syntax.md
