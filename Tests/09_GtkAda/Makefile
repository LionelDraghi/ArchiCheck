all: check doc

TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4

header:
	${TR} clean
	${TR} create "GtkAda test suite"
	- @ rm -rf gtkada-master
	@ unzip -q gtkada-master.zip

test1:
	${TR} start "File Identification"		
	
	${TR} cmt "Checking that"
	${TR} cmt 
	${TR} cmt "> archicheck -q -lf -r -I gtkada-master"							
	${TR} cmt 
	${TR} cmt "is identifying the same `cat expected_output.1 | wc -l` files (once sorted) than"							
	${TR} cmt 
	${TR} cmt "> find gtkada-master -name ""*.ad[sb]"" | sort"							
	@ ${TR} cmt '```'
	head -n 3 expected_output.1 >> testrec.md
	echo '...'					>> testrec.md
	tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	${TR} cmt 
	find gtkada-master -name "*.ad[sb]"	| sort > expected_output.1
	../../Obj/archicheck -q -lf -r -I gtkada-master	| sed "s/.*gtkada-master/gtkada-master/" | sort	> output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end

test2:
	${TR} start "Unit Identification"		
	
	# to do a real check here, I need as a reference an ASIS tools that list all dependency
	${TR} cmt 
	${TR} cmt "> archicheck -ld -r -I gtkada-master | sort"							
	${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 10 expected_output.2 >> testrec.md
	echo '...'				  	 >> testrec.md
	tail -n 10 expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt

	../../Obj/archicheck -ld -r -I gtkada-master | sort > output.2
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	${TR} start "A realistic GtkAda description file"		
	
	${TR} cmt 
	${TR} cmt '![](gtk.png)'
	${TR} cmt 
	${TR} cmt "Checking those rules over GtkAda:"							
	${TR} cmt 
	${TR} cmt '```'							
	cat GtkAda.rules		>> testrec.md						
	${TR} cmt '```'
	${TR} cmt 

	${TR} cmt "Expected output:"			
	${TR} cmt 
	${TR} cmt '```'							
	cat expected_output.3		>> testrec.md						
	${TR} cmt '```'
	${TR} cmt 
								
	../../Obj/archicheck GtkAda.rules -r -I gtkada-master	> output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end


test4:
	${TR} start "Another realistic GtkAda description file"		
	
	${TR} cmt 
	${TR} cmt "Checking those rules over GtkAda:"							
	${TR} cmt 
	${TR} cmt '```'							
	cat GtkAda2.rules		>> testrec.md						
	${TR} cmt '```'
	${TR} cmt 

	${TR} cmt "Expected output:"			
	${TR} cmt 
	${TR} cmt '```'							
	cat expected_output.4		>> testrec.md						
	${TR} cmt '```'
	${TR} cmt 
								
	../../Obj/archicheck GtkAda2.rules -q -r -I gtkada-master	> output.4
	# Fixme: -q is set to avoid warnings and focus on error till code is fixed
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end


clean:
	- ${RM} -rf output.? gtkada-master
	${TR} clean

doc:
	cp testrec.md 	../../docs/tests/gtkada.md
	cp gtk.png	../../docs/tests/