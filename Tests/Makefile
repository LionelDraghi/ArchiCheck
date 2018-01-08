all: check doc

.PHONY : clean

TESTS=01_Command_Line 02_Source_List 03_Dependency_List 04_Component_List 05_Layer_Rule 06_Child_Packages 07_Rules_Files_Syntax 08_Globbing_Characters 09_GtkAda 10_Use_Rule 11_Batik 12_ZipAda 13_Ada_Units 14_Rules_On_Components 15_Precedences_Rules 16_AdaControl

Tools/create_pkg:
	@ $(MAKE) create_pkg --directory=Tools

Tools/testrec:
	@ $(MAKE) testrec    --directory=Tools

check: ../Obj/archicheck Tools/create_pkg Tools/testrec
    # for TEST in $(TESTS); do \
    #     $(MAKE) check $$TEST; \
    # done
	$(MAKE) check --directory=01_Command_Line
	$(MAKE) check --directory=02_Source_List
	$(MAKE) check --directory=03_Dependency_List
	$(MAKE) check --directory=04_Component_List
	$(MAKE) check --directory=05_Layer_Rule
	$(MAKE) check --directory=06_Child_Packages
	$(MAKE) check --directory=07_Rules_Files_Syntax
	$(MAKE) check --directory=08_Globbing_Characters
	$(MAKE) check --directory=09_GtkAda
	$(MAKE) check --directory=10_Use_Rule
	$(MAKE) check --directory=11_Batik
	$(MAKE) check --directory=12_ZipAda
	$(MAKE) check --directory=13_Ada_Units
	$(MAKE) check --directory=14_Rules_On_Components
	$(MAKE) check --directory=15_Precedences_Rules
	$(MAKE) check --directory=16_AdaControl
	$(MAKE) check --directory=17_ArchiCheck
	$(MAKE) check --directory=18_Spring_PetClinic

doc:
	cat `find . -name testrec.md` > tmp.txt
	- grep '\[Successful\]'  tmp.txt > Successful.lst
	- grep '\[Failed\]'      tmp.txt > Failed.lst
	- grep '\[Empty\]'       tmp.txt > Empty.lst
	rm tmp.txt

	> tests_count.txt
	echo "Successful " `cat Successful.lst | wc -l`	>> tests_count.txt
	echo "Failed     " `cat Failed.lst     | wc -l`	>> tests_count.txt
	echo "Empty      " `cat Empty.lst      | wc -l`	>> tests_count.txt

	>  tests_status.md
	echo '# Tests Status'	                               	>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo '## Successful'	                               	>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Successful.lst | wc -l`" tests OK"       >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^ */  - \[/;s/ \[Successful\].*/\]\(\)/" Successful.lst	>> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Failed'                                   		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Failed.lst | wc -l`" tests failed"       >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^** /  - \[/;s/ \[Failed\].*/\]\(\)/" Failed.lst >> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Empty'                                    		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Empty.lst | wc -l`" empty tests"         >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^ */  - \[/;s/ \[Empty\].*/\]\(\)/" Empty.lst	>> tests_status.md

	mv tests_status.md ../docs/tests

	@ $(MAKE) doc --directory=01_Command_Line
	@ $(MAKE) doc --directory=02_Source_List
	@ $(MAKE) doc --directory=03_Dependency_List
	@ $(MAKE) doc --directory=04_Component_List
	@ $(MAKE) doc --directory=05_Layer_Rule
	@ $(MAKE) doc --directory=06_Child_Packages
	@ $(MAKE) doc --directory=07_Rules_Files_Syntax
	@ $(MAKE) doc --directory=08_Globbing_Characters
	@ $(MAKE) doc --directory=09_GtkAda
	@ $(MAKE) doc --directory=10_Use_Rule
	@ $(MAKE) doc --directory=11_Batik
	@ $(MAKE) doc --directory=12_ZipAda
	@ $(MAKE) doc --directory=13_Ada_Units
	@ $(MAKE) doc --directory=14_Rules_On_Components
	@ $(MAKE) doc --directory=15_Precedences_Rules
	@ $(MAKE) doc --directory=16_AdaControl
	@ $(MAKE) doc --directory=17_ArchiCheck
	@ $(MAKE) doc --directory=18_Spring_PetClinic

clean:
	@ $(MAKE) clean --directory=Tools

	@ $(MAKE) clean --directory=01_Command_Line
	@ $(MAKE) clean --directory=02_Source_List
	@ $(MAKE) clean --directory=03_Dependency_List
	@ $(MAKE) clean --directory=04_Component_List
	@ $(MAKE) clean --directory=05_Layer_Rule
	@ $(MAKE) clean --directory=06_Child_Packages
	@ $(MAKE) clean --directory=07_Rules_Files_Syntax
	@ $(MAKE) clean --directory=08_Globbing_Characters
	@ $(MAKE) clean --directory=09_GtkAda
	@ $(MAKE) clean --directory=10_Use_Rule
	@ $(MAKE) clean --directory=11_Batik
	@ $(MAKE) clean --directory=12_ZipAda
	@ $(MAKE) clean --directory=13_Ada_Units
	@ $(MAKE) clean --directory=14_Rules_On_Components
	@ $(MAKE) clean --directory=15_Precedences_Rules
	@ $(MAKE) clean --directory=16_AdaControl
	@ $(MAKE) clean --directory=17_ArchiCheck
	@ $(MAKE) clean --directory=18_Spring_PetClinic
	
	- ${RM} tests_status.md *.lst short_tests_status.md tests_count.txt


