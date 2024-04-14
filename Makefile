#-- -----------------------------------------------------------------------------
#-- Acc, the software architecture compliance verifier
#-- Copyright (C) Lionel Draghi
#-- This program is free software;
#-- you can redistribute it and/or modify it under the terms of the GNU General
#-- Public License Versions 3, refer to the COPYING file.
#-- This file is part of ArchiCheck : http://lionel.draghi.free.fr/Archicheck/archicheck
#-- -----------------------------------------------------------------------------

.SILENT:

## mkfile := $(abspath $(lastword $(MAKEFILE_LIST)))
## rootdir := $(dir $(patsubst %/,%,$(dir $(mkfile))))

all: build tools check doc

release: build_release 
	@ echo Make release build

	@ > docs/download.md
	@ echo "Download"			 									>> docs/download.md
	@ echo "========"			 									>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck)"	>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "build on :"												>> docs/download.md
	@ echo "----------"												>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "> uname -orm" 											>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ uname -orm		 											>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "> gnat --version | head -1"								>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ gnat --version | head -1										>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ echo "and -O3 option." 										>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '(May be necessary after download : `chmod +x acc`)'	    >> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "Exe check :"											>> docs/download.md
	@ echo "-----------"											>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "> date -r acc --iso-8601=seconds" 						>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ date -r obj/acc --iso-8601=seconds 							>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "> readelf -d acc | grep 'NEEDED'" 						>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ readelf -d obj/acc | grep 'NEEDED'							>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "> acc --version"				 						>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ obj/acc --version				 								>> docs/download.md
	@ echo '```' 													>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "Tests status on this exe :"								>> docs/download.md
	@ echo "--------------------------"								>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ cat release_tests.txt											>> docs/download.md
	
	@ cp -rp obj/acc docs/
	@ cp -rp obj/acc ~/bin
	@ rm release_tests.txt

build: 
	@ echo Make debug build
	@ - mkdir -p obj lib

	@ # alr gnatcov instrument --no-subprojects --level=stmt --dump-trigger=atexit --projects archicheck.gpr
	@ # alr build -- -q -s -Xmode=debug --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full
	alr build --development 
	@ # -q : quiet
	@ # -s : recompile if compiler switches have changed

.PHONY : build_release
build_release:
	alr build --release
	# -q : quiet
	# -s : recompile if compiler switches have changed

	# equal to check, but without coverage :
	@ echo - Running tests :
	@ ## $(MAKE) --ignore-errors --directory=Tests
	@ $(MAKE) --directory=Tests

	@ echo "Run "`date --iso-8601=seconds` 	>  release_tests.txt
	@ echo									>> release_tests.txt
	@ sed "s/^/- /" Tests/tests_count.txt	>> release_tests.txt

	echo
	@ echo - Tests summary :
	@ cat Tests/tests_count.txt

tools: 
	@ $(MAKE) create_pkg --directory=Tools
	@ $(MAKE) testrec    --directory=Tools

check: obj/acc
	# depend on the exe, may be either build or build_release, test have to pass with both
	@ echo Make check
	##@ - mkdir -p Tools/obj 

	@ echo - Initializing coverage data before run
	# lcov --quiet --capture --initial --directory obj -o obj/coverage.info --ignore-errors source
	# lcov error are ignored because this is also runned when in release mode,  
	# without coverage info generated

	@ echo - Running tests :
	## $(MAKE) --ignore-errors --directory=Tests
	$(MAKE) --directory=Tests

	echo
	@ echo - Tests summary :
	@ cat Tests/tests_count.txt

	# --------------------------------------------------------------------
	echo
	@ echo - Coverage report :

	# @ lcov --quiet --capture --directory obj -o obj/coverage.info --ignore-errors source
	# @ lcov --quiet --remove obj/coverage.info -o obj/coverage.info \
	# 	"*/adainclude/*" "*/src_opentoken-6.0b/*" "*.ads" "*/obj/b__archicheck-main.adb"
	# 	# "*/patch_opentoken/*" 
	# Ignoring :
	# - spec (results are not consistent with current gcc version) 
	# - the false main
	# - libs (Standard)
	# - OpenToken

	@ # Summary table in md format :
	# @ lcov --list obj/coverage.info > docs/coverage_summary.md


	# @ genhtml obj/coverage.info -o docs/lcov --title "ArchiCheck tests coverage" \
	# 	--show-navigation --function-coverage --branch-coverage \
	# 	--prefix "/home/lionel/Projets/Logiciels/Archicheck" --frames | tail -n 2 > cov_sum.txt
	# --title  : Display TITLE in header of all pages
	# --prefix : Remove PREFIX from all directory names
	# --frame  : Use HTML frames for source code view
	# @ cat cov_sum.txt
	@ echo

.PHONY : dashboard
dashboard: Tests/tests_count.txt
	@ echo Make dashboard

	@ # Language pie
	@ # --------------------------------------------------------------------
	@ sloccount src Tests/Tools | grep "ada=" |  ploticus  -prefab pie 	\
		data=stdin labels=2 colors="blue red green orange"		\
		explode=0.1 values=1 title="Ada sloc `date +%x`"		\
		-png -o docs/generated_img/sloc.png

	@ # Code coverage Pie

	# Processing the lines line :
	@ # --------------------------------------------------------------------
	# > lines_cov.dat
	# head -n 1 cov_sum.txt | sed "s/.*(/\"Covered lines\" /" | sed "s/ of .*//"		>> lines_cov.dat
	# head -n 1 cov_sum.txt | sed "s/.* of /\"Total   lines\" /" | sed "s/ lines)//"	>> lines_cov.dat
	# ploticus -prefab pie 						\
	# 	data=lines_cov.dat labels=1 colors="green blue" 	\
	# 	explode=0.1 values=2 title="Lines coverage `date +%x`"	\
	# 	labelfmtstring=@2 -png -o docs/generated_img/lines_coverage.png
	
	# Processing the functions line :
	@ # --------------------------------------------------------------------
	# > functions_cov.dat
	# tail -n 1 cov_sum.txt | sed "s/.*(/\"Covered functions\" /" | sed "s/ of .*//"			>> functions_cov.dat
	# tail -n 1 cov_sum.txt | sed "s/.* of /\"Total   functions\" /" | sed "s/ functions)//"	>> functions_cov.dat
	# ploticus -prefab pie data=functions_cov.dat labels=1 colors="green blue" 	\
	# 	explode=0.1 values=2 title="Functions coverage `date +%x`"		\
	# 	labelfmtstring=" @2\\n (@PCT%)" -png -o docs/generated_img/functions_coverage.png
	
	@ # Test pie	
	@ # --------------------------------------------------------------------
	@ ploticus -prefab pie legend=yes							\
		data=Tests/tests_count.txt labels=1 colors="green red orange"	\
		explode=0.1 values=2 title="Tests results `date +%x`"			\
		-png -o docs/generated_img/tests.png

	>  docs/dashboard.md
	@ echo "Dashboard"								>> docs/dashboard.md
	@ echo "========="								>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md
	@ echo "Version"								>> docs/dashboard.md
	@ echo "-------"								>> docs/dashboard.md
	@ echo "> acc --version"						>> docs/dashboard.md
	@ echo 	 										>> docs/dashboard.md
	@ echo '```' 									>> docs/dashboard.md
	@ obj/acc --version 							>> docs/dashboard.md
	@ echo '```' 									>> docs/dashboard.md
	@ echo 	 										>> docs/dashboard.md
	@ echo "> date -r acc --iso-8601=seconds" 		>> docs/dashboard.md
	@ echo 	 										>> docs/dashboard.md
	@ echo '```' 									>> docs/dashboard.md
	@ date -r obj/acc --iso-8601=seconds 			>> docs/dashboard.md
	@ echo '```' 									>> docs/dashboard.md
	@ echo 	 										>> docs/dashboard.md
	@ echo "Test results"							>> docs/dashboard.md
	@ echo "------------"							>> docs/dashboard.md
	@ echo '```'			 						>> docs/dashboard.md
	@ cat Tests/tests_count.txt						>> docs/dashboard.md
	@ echo '```'			 						>> docs/dashboard.md
	@ echo "![](generated_img/tests.png)"			>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md
	@ echo "Coverage"								>> docs/dashboard.md
	@ echo "--------"								>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md
	@ echo '```'			 						>> docs/dashboard.md
	#### @ cat cov_sum.txt							>> docs/dashboard.md
	@ echo '```'			 						>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md
	@ cat docs/coverage_summary.md					>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md
	@ echo '[**Coverage details in the sources**](http://lionel.draghi.free.fr/Archicheck/lcov/home/lionel/Proj/Archicheck/src/index-sort-f.html)'	>> docs/dashboard.md
	@ echo 											>> docs/dashboard.md

	# badge making:
	@ wget -q "https://img.shields.io/badge/Version-`./obj/acc --version`-blue.svg" -O docs/generated_img/version.svg
	@ wget -q "https://img.shields.io/badge/Tests_OK-`cat Tests/tests_count.txt |sed -n "s/Successful  //p"`-green.svg" -O docs/generated_img/tests_ok.svg
	@ wget -q "https://img.shields.io/badge/Tests_KO-`cat Tests/tests_count.txt |sed -n "s/Failed      //p"`-red.svg" -O docs/generated_img/tests_ko.svg

.PHONY : cmd_line.md
cmd_line.md:
	@ echo Make cmd_line.md
	@ > docs/cmd_line.md
	@ echo "Archicheck command line"	>> docs/cmd_line.md
	@ echo "======================="	>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo "Archicheck command line"	>> docs/cmd_line.md
	@ echo "-----------------------"	>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo "$ acc -h" 					>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ obj/acc -h 						>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo "Archicheck current version"	>> docs/cmd_line.md
	@ echo "--------------------------"	>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo "$ acc --version"			>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ obj/acc --version					>> docs/cmd_line.md
	@ echo '```'						>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md

doc: dashboard cmd_line.md
	@ echo Make Doc
	
	@ >  docs/fixme.md
	@ rgrep -ni "Fixme" docs/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	@ echo 'Fixme in current version:'		>> docs/fixme.md
	@ echo '-------------------------'		>> docs/fixme.md
	@ echo                            		>> docs/fixme.md
	@ echo 'Location | Text'             	>> docs/fixme.md
	@ echo '---------|-----'             	>> docs/fixme.md
	@ cat /tmp/fixme.md                     >> docs/fixme.md
	@ rm  /tmp/fixme.md
	@ rgrep -ni              "Fixme" src/*     | sed "s/:/|/2"	>> docs/fixme.md
	@ grep -ni --no-messages "Fixme" Tests/*/* | sed "s/:/|/2"	>> docs/fixme.md

	@ mkdocs build --clean
	@ - chmod --silent +x ./site/archicheck
    
.PHONY : clean
clean:
	@ echo Make clean
	@ alr clean
	@ - ${RM} -rf obj/* docs/lcov/* tmp.txt *.lst *.dat cov_sum.txt gmon.out gh-md-toc docs/generated_img/*
	@ - $(MAKE) --directory=Tests clean
	@ - $(MAKE) --directory=Tools clean
	