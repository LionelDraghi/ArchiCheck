#-- -----------------------------------------------------------------------------
#-- Acc, the software architecture compliance verifier
#-- Copyright (C) Lionel Draghi
#-- This program is free software;
#-- you can redistribute it and/or modify it under the terms of the GNU General
#-- Public License Versions 3, refer to the COPYING file.
#-- This file is part of ArchiCheck : https://github.com/LionelDraghi/ArchiCheck
#-- -----------------------------------------------------------------------------

.SILENT:

# Tests counts, extracted from the bbt consolidated results summary table
TESTS_COUNT  = grep -E '^\| (Successful|Failed) ' docs/tests/test_results.md | sed 's/|//g;s/^ *//;s/ *$$//;s/  */ /g'
TESTS_STATUS = grep -E '^\| (Failed|Successful|Empty|Not Run)' docs/tests/test_results.md | sed 's/|//g;s/^ *//;s/ *$$//;s/  */ /g'

## mkfile := $(abspath $(lastword $(MAKEFILE_LIST)))
## rootdir := $(dir $(patsubst %/,%,$(dir $(mkfile))))

all: build tools check doc

.PHONY : help
help:
	@ echo "Usage: make [target]"
	@ echo ""
	@ echo "Targets:"
	@ echo "  all           : build, tools, check and doc (default when no target given)"
	@ echo "  build         : build acc in development mode"
	@ echo "  build_release : build acc in release mode"
	@ echo "  release       : full release chain (release build, tests, badges,"
	@ echo "                   docs/download.md, install in ~/bin)"
	@ echo "  tools         : build the Tools sub-project (create_pkg)"
	@ echo "  check         : run the bbt test suites"
	@ echo "  cmd_line.md   : regenerate docs/cmd_line.md"
	@ echo "  doc           : regenerate the generated docs (fixme index, tests"
	@ echo "                   doc, cmd_line.md, badges)"
	@ echo "  clean         : remove the build and test artifacts"
	@ echo ""
	@ echo "Refer to AGENTS.md and docs/building.md for more details."

release: build_release 
	@ echo Make release build

	@ > docs/download.md
	@ echo "Download"			 									>> docs/download.md
	@ echo "========"			 									>> docs/download.md
	@ echo 	 														>> docs/download.md
	@ echo "[Download Linux exe](https://github.com/LionelDraghi/ArchiCheck/releases)"	>> docs/download.md
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
	
	@ cp -rp obj/acc ~/bin
	@ rm release_tests.txt

build: 
	@ echo Make debug build
	@ - mkdir -p obj lib

	alr build --development 
	@ # -q : quiet
	@ # -s : recompile if compiler switches have changed

# The check target depends on the obj/acc file, that may come from either
# build or build_release: build it (in development mode) only when missing.
obj/acc:
	@ echo No obj/acc found, making a development build
	@ $(MAKE) build

.PHONY : build_release
build_release:
	alr build --release
	# -q : quiet
	# -s : recompile if compiler switches have changed

	@ echo - Running tests :
	@ ## $(MAKE) --ignore-errors --directory=Tests
	@ $(MAKE) --directory=Tests

	@ echo "Run "`date --iso-8601=seconds` 	>  release_tests.txt
	@ echo									>> release_tests.txt
	@ $(TESTS_STATUS) | sed "s/^/- /"	>> release_tests.txt

	echo
	@ echo - Tests summary :
	@ $(TESTS_STATUS)

tools: 
	@ $(MAKE) create_pkg --directory=Tools

check: obj/acc tools
	# depend on the exe, may be either build or build_release, test have to pass with both
	@ echo Make check
	##@ - mkdir -p Tools/obj 

	@ echo - Running tests :
	## $(MAKE) --ignore-errors --directory=Tests
	$(MAKE) --directory=Tests

	echo
	@ echo - Tests summary :
	@ $(TESTS_STATUS)


.PHONY : badges
badges: docs/tests/test_results.md
	@ echo Make badges

	# badge making:
	@ wget -q "https://img.shields.io/badge/Version-`./obj/acc --version`-blue.svg" -O docs/generated_img/version.svg
	@ wget -q "https://img.shields.io/badge/Tests_OK-`grep '| Successful' docs/tests/test_results.md | grep -o '[0-9]\+'`-green.svg" -O docs/generated_img/tests_ok.svg
	@ wget -q "https://img.shields.io/badge/Tests_KO-`grep '| Failed' docs/tests/test_results.md | grep -o '[0-9]\+'`-red.svg" -O docs/generated_img/tests_ko.svg

.PHONY : cmd_line.md
cmd_line.md:
	@ echo Make cmd_line.md
	@ > docs/cmd_line.md
	@ echo "Acc command line"	>> docs/cmd_line.md
	@ echo "======================="	>> docs/cmd_line.md
	@ echo ""							>> docs/cmd_line.md
	@ echo "Acc command line"	>> docs/cmd_line.md
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
	@ echo "Acc current version"	>> docs/cmd_line.md
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

doc: badges cmd_line.md
	@ echo Make Doc
	
	@ >  docs/fixme.md
	@ rgrep -ni "Fixme:" docs/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	@ echo 'Fixme in current version:'		>> docs/fixme.md
	@ echo '-------------------------'		>> docs/fixme.md
	@ echo                            		>> docs/fixme.md
	@ echo 'Location | Text'             	>> docs/fixme.md
	@ echo '---------|-----'             	>> docs/fixme.md
	@ cat /tmp/fixme.md                     >> docs/fixme.md
	@ rm  /tmp/fixme.md
	@ rgrep -ni              "Fixme:" src/*     | sed "s/:/|/2"	>> docs/fixme.md
	@ grep -ni --no-messages "Fixme:" Tests/*/* | sed "s/:/|/2"	>> docs/fixme.md

.PHONY : clean
clean:
	@ echo Make clean
	@ alr clean
	@ - ${RM} -rf obj/* tmp.txt *.lst *.dat gmon.out *.md.out gh-md-toc
	@ - $(MAKE) --directory=Tests clean
	@ - $(MAKE) --directory=Tools clean
	