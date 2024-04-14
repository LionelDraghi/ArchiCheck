all: check doc

MD=command_line.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2 test3 test4 test5 test6 test8 test9 test10 test11 test12 test13 test14 test15

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
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ ../../obj/acc -h > expected_help.txt
	@ cat expected_help.txt >> ${MD}
	@ ${TR} cmt '```' 						
	@ ${TR} cmt 

	@ ../../obj/acc --help	> help1.txt
	@ ${TR} assert true /usr/bin/sdiff "-sbB help1.txt expected_help.txt" 
	
	@ ../../obj/acc  		> help2.txt 
	@ ${TR} assert true /usr/bin/sdiff "-sbB help2.txt expected_help.txt" 

	@ ${TR} end
	
test2:
	@ ${TR} start "Version option" 					
	@ ${TR} cmt  								
	@ ${TR} cmt "Test that the --version will put :" 			
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "6.0.0"	            > expected_version.txt
	@ cat expected_version.txt >> ${MD}					
	@ ${TR} cmt '```' 						
	@ ${TR} cmt 

	@ ../../obj/acc --version	> version1.txt
	@ ${TR} assert true /usr/bin/sdiff "-sbB version1.txt expected_version.txt"

	@ ${TR} end
	
test3:
	@ ${TR} start "-I option without src dir" 				
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -I"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Error : Sources directory expected after -I"	>  expected_err3.txt
	@ cat expected_err3.txt									>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/acc -I		> err3.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err3.txt err3.txt"

	@ ${TR} end
	
test4:
	@ ${TR} start "-I option with an unknown dir" 			
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -I qsdqjh"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt
	@ ${TR} cmt '```'
	@ echo "Error : No qsdqjh directory"	>  expected_err4.txt
	@ cat expected_err4.txt					>> ${MD}					
	@ ${TR} cmt '```'
	@ ${TR} cmt 							

	@ test ! `../../obj/acc -I qsdqjh		> err4.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err4.txt err4.txt"
  
	@ ${TR} end
	
test5:
	@ ${TR} start "unknown -xyz option" 				
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -xzy"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Error : Unknown rules file or unknown option -xzy"	>  expected_err5.txt
	@ cat expected_err5.txt										>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/acc -xzy		> err5.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err5.txt err5.txt"
  
	@ ${TR} end

test6:
	@ ${TR} start "-I option with... nothing to do" 			
	@ ${TR} cmt "(no rules file, no -ld or -lf, etc.)"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir -p dir6  "
	@ ${TR} cmt "> touch dir6/src.adb  "
	@ ${TR} cmt "> acc -I dir6  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ echo "Error : Nothing to do with those sources"	>  expected_err6.txt
	@ cat expected_help.txt 							>> expected_err6.txt
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err6.txt	>> ${MD}					
	@ ${TR} cmt '```' 
	@ ${TR} cmt 

	@ mkdir -p dir6
	@ touch dir6/src.adb
	@ test ! `../../obj/acc -I dir6		> err6.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err6.txt err6.txt"
	  
	@ ${TR} end
	
## test7:

test8:
	@ ${TR} start "-lr option without rules file" 			
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -lr"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ echo "Error : No rules file given"	>  expected_err8.txt
	@ cat expected_help.txt 				>> expected_err8.txt
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ cat expected_err8.txt	>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/acc -lr  	> err8.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err8.txt err8.txt"
  
	@ ${TR} end

test9:
	@ ${TR} start "Legal line, but no src file in the given (existing) directory"
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir dir9  "
	@ ${TR} cmt "> acc -lf -I dir9  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Warning : Cannot list files, no sources found to analyze"	>  expected_err9.txt	
	@ cat expected_err9.txt												>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt 
 							
	@ mkdir -p dir9
	@ ../../obj/acc -lf -I dir9 	> err9.txt
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"

	@ ${TR} cmt "archicheck return 1 if -We or --Warnings=error"		
	@ test ! `../../obj/acc -We -lf -I dir9 	> err9.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"
	@ test ! `../../obj/acc --Warnings=error -lf -I dir9 	> err9.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err9.txt err9.txt"


	@ ${TR} end

test10:
	@ ${TR} start "file given to -I, instead of a directory"
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> touch rules.txt src.adb  "
	@ ${TR} cmt "> acc rules.txt -I src.adb  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Error : src.adb is not a directory"	>  expected_err10.txt	
	@ cat expected_err10.txt					>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ touch rules.txt src.adb
	@ test ! `../../obj/acc rules.txt -I src.adb 	> err10.txt 2>&1`
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
	@ ${TR} cmt "> acc rules.txt -ld -I dir11"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Warning : Cannot list dependencies, no sources found"	>  expected_err11.txt
	@ cat expected_err11.txt										>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ echo "Interfaces use is forbidden" > rules.txt 
	@ mkdir -p dir11
	@ ../../obj/acc rules.txt -ld -I dir11 	> err11.txt 
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err11.txt err11.txt"
  
	@ ${TR} end
	
test12:
	@ ${TR} start "src found, but nothing to do whith it"		
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> mkdir -p dir12  "
	@ ${TR} cmt "> touch dir12/src.adb  "
	@ ${TR} cmt "> acc -I dir12  "		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Error : Nothing to do with those sources"	>  expected_err12.txt
	@ cat expected_help.txt 							>> expected_err12.txt
	@ cat expected_err12.txt							>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  
							
	@ mkdir -p dir12
	@ touch dir12/src.adb
	@ test ! `../../obj/acc -I dir12			> err12.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err12.txt err12.txt"
  
	@ ${TR} end
	
test13:
	@ ${TR} start "rules file found, but nothing to do whith it"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc rules.txt"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```' 						
	@ echo "Error : Nothing to do with this rules file"	>  expected_err13.txt
	@ cat expected_help.txt 					        >> expected_err13.txt
	@ cat expected_err13.txt							>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  
							
	@ test ! `../../obj/acc rules.txt				> err13.txt 2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err13.txt err13.txt"
  
	@ ${TR} end
	
test14:
	@ ${TR} start "template creation (-ct and --create_template)"	
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -ct"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting file template.ac:"		
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_template.ac	>> ${MD}
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  

	@ - ${RM} template.ac	
	@ ../../obj/acc -ct
	@ ${TR} assert true /usr/bin/sdiff "-s expected_template.ac template.ac"

	@ ${TR} cmt "When running once more :"		
	@ ${TR} cmt "> acc --create_template"		
	@ ${TR} cmt "Expecting error:"		
	@ ${TR} cmt '```'
	@ echo "Error : File template.ac already exists"	>  expected_err14.txt
	@ cat expected_err14.txt							>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  

	@ test ! `../../obj/acc --create_template				 2>&1 > err14.txt`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err14.txt err14.txt"
  
	@ ${TR} end
	
test15:
	@ ${TR} start "-ar without rule " 				
	@ ${TR} cmt
	@ ${TR} cmt "When running:"		
	@ ${TR} cmt "> acc -ar"		
	@ ${TR} cmt "or"		
	@ ${TR} cmt "> acc --apend_rule"		
	@ ${TR} cmt 
	@ ${TR} cmt "Expecting:"		
	@ ${TR} cmt '```' 						
	@ echo "Error : Rule expected after --append_rule"	>  expected_err15.txt
	@ cat expected_err15.txt							>> ${MD}					
	@ ${TR} cmt '```' 						 	
	@ ${TR} cmt  							

	@ test ! `../../obj/acc -ar				> err15a.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err15.txt err15a.txt"
	@ test ! `../../obj/acc --append_rule	> err15b.txt  2>&1`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_err15.txt err15b.txt"

	@ ${TR} end
	
clean:
	@ - ${RM} -rf dir? dir?? err*.txt expected_err*.txt expected_help.txt expected_version.txt src.adb rules.txt version1.txt help?.txt template.ac 
	@ ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
