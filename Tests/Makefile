all: check tests_summary.txt

.PHONY : clean

Tools/create_pkg:
	@ $(MAKE) create_pkg --directory=Tools

Tools/testrec:
	@ $(MAKE) testrec    --directory=Tools

check: ../Obj/archicheck Tools/create_pkg Tools/testrec
	$(MAKE) check --directory=01_Command_Line
	$(MAKE) check --directory=02_Source_List
	$(MAKE) check --directory=03_Dependency_List
	$(MAKE) check --directory=04_Component_List
	$(MAKE) check --directory=05_Layer_Rule
	$(MAKE) check --directory=06_Child_Packages
	$(MAKE) check --directory=07_Rules_Files_Syntax
	$(MAKE) check --directory=08_Globbing_Characters
	$(MAKE) check --directory=09_GtkAda

tests_summary.txt:
	cat `find . -name testrec.txt` > tmp.txt
	- grep "<Successful>"  tmp.txt > Successful.lst
	- grep "<Failed>"      tmp.txt > Failed.lst
	- grep "<Empty>"       tmp.txt > Empty.lst
	rm tmp.txt

	> short_tests_summary.txt
	echo "Successful " `cat Successful.lst | wc -l`	>> short_tests_summary.txt
	echo "Failed     " `cat Failed.lst     | wc -l`	>> short_tests_summary.txt
	echo "Empty      " `cat Empty.lst      | wc -l`	>> short_tests_summary.txt

	echo "File: Test Status"                                >  tests_summary.txt

	echo ""                                                 >> tests_summary.txt
	echo "Result: Successful"                               >> tests_summary.txt
	echo "   "`cat Successful.lst | wc -l`" tests OK"       >> tests_summary.txt
	sed "s/^ */   - </;s/ <Successful.*/>/" Successful.lst  >> tests_summary.txt
	echo ""                                                 >> tests_summary.txt

	echo ""                                                 >> tests_summary.txt
	echo "Result: Failed"                                   >> tests_summary.txt
	echo "   "`cat Failed.lst | wc -l`" tests failed"       >> tests_summary.txt
	sed "s/^ */   - </;s/ <Failed.*/>/" Failed.lst          >> tests_summary.txt

	echo ""                                                 >> tests_summary.txt
	echo "Result: Empty"                                    >> tests_summary.txt
	echo "   "`cat Empty.lst | wc -l`" empty tests"         >> tests_summary.txt
	sed "s/^ */   - </;s/ <Empty.*/>/" Empty.lst            >> tests_summary.txt
	echo ""                                                 >> tests_summary.txt

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

	- ${RM} tests_summary.txt *.lst short_tests_summary.txt


