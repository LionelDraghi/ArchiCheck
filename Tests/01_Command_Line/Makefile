all: check

TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4 test5 test6 test8 test9 test10 test11 test12 test13 

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Command line test suite"

	${TR} cmt  								
	${TR} cmt "This test check that illegal command lines cause archicheck to"	
	${TR} cmt "exit with a non null return code." 				
	${TR} cmt "- Note that normal use is overly tested in other tests," 		
	${TR} cmt "  so here mainly error situations are tested."			
	${TR} cmt "- Note also that quiet and verbose mode (-q / -v) are also tested"	
	${TR} cmt "  in other tests."							
	${TR} cmt  								

test1:
	${TR} start "Help options:" 
	${TR} cmt  								
	${TR} cmt "   Test that the -h, --help or no command line will output :" 	
	${TR} cmt  								
	
	../../Obj/archicheck -h > expected_help.txt
	${TR} cmt "(start code)" 						
	cat expected_help.txt >> testrec.txt
	${TR} cmt "(end)" 							

	../../Obj/archicheck --help	> help1.txt
	${TR} assert true /usr/bin/sdiff "-sbB help1.txt expected_help.txt" 
	
	../../Obj/archicheck  		> help2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB help2.txt expected_help.txt" 

	${TR} end
	
test2:
	${TR} start "Version option" 					
	${TR} cmt  								
	${TR} cmt "Test that the --version will put :" 			
	${TR} cmt  								
	${TR} cmt "(start code)" 						
	echo "0.4.0"			> expected_version.txt
	cat expected_version.txt >> testrec.txt					
	${TR} cmt "(end)" 							

	../../Obj/archicheck --version	> version1.txt
	${TR} assert true /usr/bin/sdiff "-sbB version1.txt expected_version.txt"

	${TR} end
	
test3:
	${TR} start "-I option without src dir" 				
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> archicheck -I"		
	${TR} cmt "Expecting:"		

	echo "Error : Sources directory expected after -I"	>  expected_err3.txt

	${TR} cmt "(start code)" 						
	cat expected_err3.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	test ! `../../Obj/archicheck -I		> err3.txt  2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err3.txt err3.txt"

	${TR} end
	
test4:
	${TR} start "-I option with an unknow dir" 			
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> archicheck -I qsdqjh"		
	${TR} cmt "Expecting:"		

	echo "Error : No qsdqjh directory"	>  expected_err4.txt

	${TR} cmt "(start code)" 						
	cat expected_err4.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	test ! `../../Obj/archicheck -I qsdqjh		> err4.txt  2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err4.txt err4.txt"
  
	${TR} end
	
test5:
	${TR} start "unknown -xyz option" 				
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> archicheck -xzy"		
	${TR} cmt "Expecting:"		

	echo "Error : Unknown rules file or unknow option -xzy"	>  expected_err5.txt
	cat expected_help.txt 					>> expected_err5.txt

	${TR} cmt "(start code)" 						
	cat expected_err5.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	test ! `../../Obj/archicheck -xzy		> err5.txt  2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err5.txt err5.txt"
  
	${TR} end

test6:
	${TR} start "-I option with... nothing to do" 			
	${TR} cmt "      (no rules file, no -ld or -lf, etc.)"	
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> mkdir -p dir6"
	${TR} cmt "> touch dir6/src.adb"
	${TR} cmt "> archicheck -I dir6"		
	${TR} cmt "Expecting:"		

	echo "Error : Nothing to do with those sources"	>  expected_err6.txt
	cat expected_help.txt 				>> expected_err6.txt

	${TR} cmt "(start code)" 						
	cat expected_err6.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	mkdir -p dir6
	touch dir6/src.adb
	test ! `../../Obj/archicheck -I dir6		> err6.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err6.txt err6.txt"
	  
	${TR} end
	
## test7:

test8:
	${TR} start "-lr option without rules file" 			
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> archicheck -lr"		
	${TR} cmt "Expecting:"		

	echo "Error : No rules file given"		>  expected_err8.txt
	cat expected_help.txt 				>> expected_err8.txt

	${TR} cmt "(start code)" 						
	cat expected_err8.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	test ! `../../Obj/archicheck -lr  	> err8.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err8.txt err8.txt"
  
	${TR} end

test9:
	${TR} start "Legal line, but no src file in the given (existing) directory"
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> mkdir dir9"
	${TR} cmt "> archicheck -lf -I dir9"		
	${TR} cmt "Expecting:"		

	echo "Error : Cannot list files, no sources found to analyze"		>  expected_err9.txt	

	${TR} cmt "(start code)" 						
	cat expected_err9.txt	>> testrec.txt					
	${TR} cmt "(end)"
 							
	mkdir -p dir9
	test ! `../../Obj/archicheck -lf -I dir9 	> err9.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"

	${TR} end

test10:
	${TR} start "file given to -I, instead of a directory"
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> touch rules.txt src.adb"
	${TR} cmt "> archicheck rules.txt -I src.adb"		
	${TR} cmt "Expecting:"		

	echo "Error : src.adb is not a directory"		>  expected_err10.txt	

	${TR} cmt "(start code)" 						
	cat expected_err10.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	touch rules.txt src.adb
	test ! `../../Obj/archicheck rules.txt -I src.adb 	> err10.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err10.txt err10.txt"
  
	${TR} end
	
test11:
	${TR} start "-ld given, but no source found"
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> mkdir -p dir11"
	${TR} cmt "> touch rules.txt"
	${TR} cmt "> archicheck rules.txt -ld -I dir11"		
	${TR} cmt "Expecting:"		

	echo "Error : Cannot list dependencies, no sources found"	>  expected_err11.txt
	cat expected_help.txt 						>> expected_err11.txt

	${TR} cmt "(start code)" 						
	cat expected_err11.txt	>> testrec.txt					
	${TR} cmt "(end)" 							

	touch rules.txt 
	mkdir -p dir11
	test ! `../../Obj/archicheck rules.txt -ld -I dir11 	> err11.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err11.txt err11.txt"
  
	${TR} end
	
test12:
	${TR} start "src found, but nothing to do whith it"		
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> mkdir -p dir12"
	${TR} cmt "> touch dir12/src.adb"
	${TR} cmt "> archicheck -I dir12"		
	${TR} cmt "Expecting:"		

	echo "Error : Nothing to do with those sources"		>  expected_err12.txt
	cat expected_help.txt 					>> expected_err12.txt

	${TR} cmt "(start code)" 						
	cat expected_err12.txt	>> testrec.txt					
	${TR} cmt "(end)" 
							
	mkdir -p dir12
	touch dir12/src.adb
	test ! `../../Obj/archicheck -I dir12			> err12.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err12.txt err12.txt"
  
	${TR} end
	
test13:
	${TR} start "rules file found, but nothing to do whith it"	
	${TR} cmt
	${TR} cmt "When :"		
	${TR} cmt "> archicheck rules.txt"		
	${TR} cmt "Expecting:"		

	echo "Error : Nothing to do with this rules file"	>  expected_err13.txt
	cat expected_help.txt 					>> expected_err13.txt
	
	${TR} cmt "(start code)" 						
	cat expected_err13.txt	>> testrec.txt					
	${TR} cmt "(end)" 
							
	test ! `../../Obj/archicheck rules.txt				> err13.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-s expected_err13.txt err13.txt"
  
	${TR} end
	
clean:
	- ${RM} -rf dir? dir?? err*.txt expected_*.txt src.adb rules.txt version1.txt help?.txt expected_*.txt 
	${TR} clean
