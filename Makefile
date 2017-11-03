.SILENT:

all: build check doc

release: clean build_release check
	cp -p Obj/archicheck Linux_amd64_release/

	> Doc/Download.txt
	echo "File: Download"			 				>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "<Here at http://lionel.draghi.free.fr/Archicheck/archicheck> is an exe build on my Debian amd64,"	>> Doc/Download.txt
	echo "with -O3 option." 						>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "About: Build" 							>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "(start code)" 							>> Doc/Download.txt
	echo "> date -r archicheck --iso-8601=seconds" 				>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	date -r Linux_amd64_release/archicheck --iso-8601=seconds 		>> Doc/Download.txt
	echo "(end)" 								>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "About: Dependencies" 						>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "(start code)" 							>> Doc/Download.txt
	echo "> readelf -d archicheck | grep 'NEEDED'" 				>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	readelf -d Linux_amd64_release/archicheck | grep 'NEEDED'		>> Doc/Download.txt
	echo "(end)" 								>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	
build: 
	echo Make debug build

	gnat make -q -s -Xmode=debug -Parchicheck.gpr
	# -q : quiet
	# -s : recompile if compiler switches have changed

build_release:
	echo Make release build

	gnat make -q -s -Xmode=release -Parchicheck.gpr
	# -q : quiet
	# -s : recompile if compiler switches have changed

check: Obj/archicheck
	# depend on the exe, may be either build or build_release, test have to pass with both
	echo Make check

	@ # initializing coverage data before run
	lcov --quiet --capture --initial --directory Obj -o Obj/coverage.info

	echo Make Tests
	$(MAKE) --directory=Tests

	@ # capturing coverage data
	lcov --quiet --capture           --directory Obj -o Obj/coverage.info


dashboard: build 
	echo Make dashboard

	@ # Language pie
	@ # --------------------------------------------------------------------
	sloccount Src Tests | grep "ada=" |  ploticus  -prefab pie 	\
		data=stdin labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		-png -o Doc/sloc.png

	@ # Code coverage Pie
	@ # --------------------------------------------------------------------
	lcov -q --remove Obj/coverage.info -o Obj/coverage.info \
		"/usr/*" "*.ads" "*/Obj/b__archicheck-main.adb"
	@ # Ignorting :
	@ # - spec (results are not consistent with current gcc version) 
	@ # - the false main
	@ # - libs (Standart and OpenToken) 

	@ # --------------------------------------------------------------------
	genhtml Obj/coverage.info -o Doc/lcov -t "ArchiCheck tests coverage" \
		-p "/home/lionel/Proj/ArchiCheck" | tail -n 2 > cov_sum.txt

	# Processing the lines line :
	@ # --------------------------------------------------------------------
	> lines_cov.dat
	head -n 1 cov_sum.txt | sed "s/.*(/\"Covered lines\" /" | sed "s/ of .*//" 	>> lines_cov.dat
	head -n 1 cov_sum.txt | sed "s/.* of /\"Total   lines\" /" | sed "s/ lines)//"	>> lines_cov.dat
	ploticus -prefab pie 	\
			data=lines_cov.dat labels=1 colors="green blue" \
			explode=0.1 values=2 title="Lines coverage `date +%x`"	\
			labelfmtstring=@2 -png -o Doc/lines_coverage.png
	
	# Processing the functions line :
	@ # --------------------------------------------------------------------
	> functions_cov.dat
	tail -n 1 cov_sum.txt | sed "s/.*(/\"Covered functions\" /" | sed "s/ of .*//" 	   	>> functions_cov.dat
	tail -n 1 cov_sum.txt | sed "s/.* of /\"Total   functions\" /" | sed "s/ functions)//" 	>> functions_cov.dat
	ploticus -prefab pie data=functions_cov.dat labels=1 colors="green blue" 		\
		explode=0.1 values=2 title="Functions coverage `date +%x`"	\
		labelfmtstring=" @2\\n (@PCT%)" -png -o Doc/functions_coverage.png
	
	@ # Test pie	
	@ # --------------------------------------------------------------------
	ploticus -prefab pie legend=yes							\
		data=Tests/short_tests_summary.txt labels=1 colors="green red orange"	\
		explode=0.1 values=2 title="Tests results `date +%x`"			\
		-png -o Doc/tests.png

	@ # --------------------------------------------------------------------
	>  Doc/Dashboard.txt
	echo "File: Dashboard"			>> Doc/Dashboard.txt
	echo 					>> Doc/Dashboard.txt
	echo "About: Test results"		>> Doc/Dashboard.txt
	echo "(start code)"			>> Doc/Dashboard.txt
	cat Tests/short_tests_summary.txt	>> Doc/Dashboard.txt
	echo "(end)"				>> Doc/Dashboard.txt
	echo "(see tests.png)"			>> Doc/Dashboard.txt
	echo 					>> Doc/Dashboard.txt
	echo "About: Lines coverage rate"	>> Doc/Dashboard.txt
	echo "(start code)"			>> Doc/Dashboard.txt
	head -n 1 cov_sum.txt			>> Doc/Dashboard.txt
	echo "(end)"				>> Doc/Dashboard.txt
	echo "(see lines_coverage.png)"		>> Doc/Dashboard.txt
	echo 					>> Doc/Dashboard.txt
	echo "About: Function coverage rate"	>> Doc/Dashboard.txt
	echo "(start code)"			>> Doc/Dashboard.txt
	tail -n 1 cov_sum.txt			>> Doc/Dashboard.txt
	echo "(end)"				>> Doc/Dashboard.txt
	echo "(see functions_coverage.png)"	>> Doc/Dashboard.txt
	echo 					>> Doc/Dashboard.txt

Cmd_Line.txt:
	echo Make Cmd_Line.txt
	> Doc/Cmd_Line.txt
	echo "File: Archicheck command line"		>> Doc/Cmd_Line.txt
	echo ""						>> Doc/Cmd_Line.txt
	echo "About: Archicheck command line"		>> Doc/Cmd_Line.txt
	echo ""						>> Doc/Cmd_Line.txt
	echo "(start code)"				>> Doc/Cmd_Line.txt
	echo "> archicheck -h" 				>> Doc/Cmd_Line.txt
	echo "(end)"					>> Doc/Cmd_Line.txt
	echo ""						>> Doc/Cmd_Line.txt
	echo "(start code)"				>> Doc/Cmd_Line.txt
	Obj/archicheck -h 				>> Doc/Cmd_Line.txt
	echo "(end)"					>> Doc/Cmd_Line.txt

	echo ""						>> Doc/Cmd_Line.txt
	echo "About: Archicheck current version"	>> Doc/Cmd_Line.txt
	echo ""						>> Doc/Cmd_Line.txt
	echo "(start code)"				>> Doc/Cmd_Line.txt
	echo "> archicheck --version"			>> Doc/Cmd_Line.txt
	echo "(end)"					>> Doc/Cmd_Line.txt
	echo ""						>> Doc/Cmd_Line.txt
	echo "(start code)"				>> Doc/Cmd_Line.txt
	Obj/archicheck --version			>> Doc/Cmd_Line.txt
	echo "(end)"					>> Doc/Cmd_Line.txt

	echo ""						>> Doc/Cmd_Line.txt

doc: dashboard Cmd_Line.txt
	echo Make Doc

	naturaldocs -q --rebuild -i Doc -i Src -i Tests 		\
		-s Default archicheck					\
		-xi _darcs						\
		-xi Src/backup						\
		-xi Obj							\
		-o FramedHTML Doc/Generated -p Doc/Natural_Docs
	cp -p  Doc/Archicheck_Overview.pdf Doc/Generated
	cp -rp Doc/lcov Doc/Generated
	cp -rp Linux_amd64_release/archicheck Doc/Generated

.PHONY : clean
clean:
	echo Make clean
	gnat clean -q -Parchicheck.gpr
	- ${RM} -rf Obj/* Doc/Generated/* Doc/lcov/* tmp.txt *.lst *.dat cov_sum.txt
	$(MAKE) --directory=Tests clean

