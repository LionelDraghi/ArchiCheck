all: check doc

TR=../../Tools/testrec

.PHONY : clean

check: ../../obj/archicheck header test1 test2 test3 test4 test5 test6 test8 test9 test10 test11 test12 test13 test14

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Command line test suite"

	@ ${TR} cmt  								
	@ ${TR} cmt "This test check that illegal command lines cause archicheck to"	
	@ ${TR} cmt "exit with a non null return code." 				
	@ ${TR} cmt "- Note that normal use is overly tested in other tests," 		
	@ ${TR} cmt "  so here mainly error situations are tested."			
	@ ${TR} cmt "- Note also that quiet and verbose mode (-q / -v) are also tested"	
	@ ${TR} cmt "  in other tests."							
	@ ${TR} cmt  								

test1:
	@ ${TR} start "Help options" 
	@ ${TR} cmt  								
	@ ${TR} cmt "Test that the -h, --help or no command line will output :" 	
	
	@ ../../obj/archicheck -h > expected_help.txt
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_help.txt >> testrec.md
	@ ${TR} cmt '```' 						
	@ ${TR} cmt 

	@ ../../obj/archicheck --help	> help1.txt
	@ ${TR} assert true /usr/bin/sdiff "-sbB help1.txt expected_help.txt" 
	
	@ ../../obj/archicheck  		> help2.txt 
	@ ${TR} assert true /usr/bin/sdiff "-sbB help2.txt expected_help.txt" 

	@ ${TR} end
	
test2:
	@ ${TR} start "Version option" 					
	@ ${TR} cmt  								
	@ ${TR} cmt "Test that the --version will put :" 			
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "0.5.8"	            > expected_version.txt
	@ cat expected_version.txt >> testrec.md					
	@ ${TR} cmt '```' 						
	@ ${TR} cmt 

	@ ../../obj/archicheck --version	> version1.txt
	@ ${TR} assert true /usr/bin/sdiff "-sbB version1.txt expected_version.txt"

	@ ${TR} end
	
test3:
	@ ${TR} start "-I option without src dir" 				
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck -I"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : Sources directory expected after -I"	>  expected_err3.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err3.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/archicheck -I		> err3.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err3.txt err3.txt"

	@ ${TR} end
	
test4:
	@ ${TR} start "-I option with an unknow dir" 			
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck -I qsdqjh"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : No qsdqjh directory"	>  expected_err4.txt

	@ ${TR} cmt
	@ ${TR} cmt '```'
	@ cat expected_err4.txt	>> testrec.md					
	@ ${TR} cmt '```'
	@ ${TR} cmt 							

	@ test ! `../../obj/archicheck -I qsdqjh		> err4.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err4.txt err4.txt"
  
	@ ${TR} end
	
test5:
	@ ${TR} start "unknown -xyz option" 				
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck -xzy"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : Unknown rules file or unknow option -xzy"	>  expected_err5.txt
	@ cat expected_help.txt 					>> expected_err5.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err5.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/archicheck -xzy		> err5.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err5.txt err5.txt"
  
	@ ${TR} end

test6:
	@ ${TR} start "-I option with... nothing to do" 			
	@ ${TR} cmt "(no rules file, no -ld or -lf, etc.)"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir -p dir6  "
	@ ${TR} cmt "> touch dir6/src.adb  "
	@ ${TR} cmt "> archicheck -I dir6  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : Nothing to do with those sources"	>  expected_err6.txt
	@ cat expected_help.txt 				>> expected_err6.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err6.txt	>> testrec.md					
	@ ${TR} cmt '```' 
	@ ${TR} cmt 

	@ mkdir -p dir6
	@ touch dir6/src.adb
	@ test ! `../../obj/archicheck -I dir6		> err6.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err6.txt err6.txt"
	  
	@ ${TR} end
	
## test7:

test8:
	@ ${TR} start "-lr option without rules file" 			
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck -lr"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : No rules file given"		>  expected_err8.txt
	@ cat expected_help.txt 				>> expected_err8.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err8.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/archicheck -lr  	> err8.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err8.txt err8.txt"
  
	@ ${TR} end

test9:
	@ ${TR} start "Legal line, but no src file in the given (existing) directory"
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir dir9  "
	@ ${TR} cmt "> archicheck -lf -I dir9  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Warning : Cannot list files, no sources found to analyze"		>  expected_err9.txt	

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err9.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt 
 							
	@ mkdir -p dir9
	@ ../../obj/archicheck -lf -I dir9 	> err9.txt
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"

	@ ${TR} cmt "archicheck return 1 if -We or --Warnings=error"		
	@ test ! `../../obj/archicheck -We -lf -I dir9 	> err9.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"
	@ test ! `../../obj/archicheck --Warnings=error -lf -I dir9 	> err9.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"


	@ ${TR} end

test10:
	@ ${TR} start "file given to -I, instead of a directory"
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> touch rules.txt src.adb  "
	@ ${TR} cmt "> archicheck rules.txt -I src.adb  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : src.adb is not a directory"		>  expected_err10.txt	

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err10.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ touch rules.txt src.adb
	@ test ! `../../obj/archicheck rules.txt -I src.adb 	> err10.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err10.txt err10.txt"
  
	@ ${TR} end
	
test11:
	@ ${TR} start "-ld given, but no source found"
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir -p dir11"
	@ ${TR} cmt
	@ ${TR} cmt "> echo ""Interfaces use is forbidden"" > rules.txt"
	@ ${TR} cmt
	@ ${TR} cmt "> archicheck rules.txt -ld -I dir11"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Warning : Cannot list dependencies, no sources found"	>  expected_err11.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err11.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ echo "Interfaces use is forbidden" > rules.txt 
	@ mkdir -p dir11
	@ ../../obj/archicheck rules.txt -ld -I dir11 	> err11.txt 
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err11.txt err11.txt"
  
	@ ${TR} end
	
test12:
	@ ${TR} start "src found, but nothing to do whith it"		
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir -p dir12  "
	@ ${TR} cmt "> touch dir12/src.adb  "
	@ ${TR} cmt "> archicheck -I dir12  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : Nothing to do with those sources"		>  expected_err12.txt
	@ cat expected_help.txt 					>> expected_err12.txt

	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err12.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  
							
	@ mkdir -p dir12
	@ touch dir12/src.adb
	@ test ! `../../obj/archicheck -I dir12			> err12.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err12.txt err12.txt"
  
	@ ${TR} end
	
test13:
	@ ${TR} start "rules file found, but nothing to do whith it"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck rules.txt"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		

	@ echo "Error : Nothing to do with this rules file"	>  expected_err13.txt
	@ cat expected_help.txt 					>> expected_err13.txt
	
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err13.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  
							
	@ test ! `../../obj/archicheck rules.txt				> err13.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err13.txt err13.txt"
  
	@ ${TR} end
	
test14:
	@ ${TR} start "template creation (-ct and --create_template)"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck -ct"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting file template.ac:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_template.ac	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  

	@ - rm  template.ac	
	@ ../../obj/archicheck -ct
	@ ${TR} assert true /usr/bin/sdiff "-s expected_template.ac template.ac"

	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> archicheck --create_template"		
	@ ${TR} cmt "Expecting error:"		
	@ ${TR} cmt '```'
	@ echo "Error : File template.ac already exists" > expected_err14.txt
	@ cat expected_err14.txt	>> testrec.md					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  

	@ ../../obj/archicheck --create_template
	@ test ! `../../obj/archicheck --create_template				 2>&1 > err14.txt`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err14.txt err14.txt"
  
	@ ${TR} end
	
clean:
	@ - ${RM} -rf dir? dir?? err*.txt expected_err*.txt expected_help.txt expected_version.txt src.adb rules.txt version1.txt help?.txt template.ac 
	@ ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/command_line.md
