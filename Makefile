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
	echo "Build:" 							>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "(start code)" 							>> Doc/Download.txt
	echo "date -r archicheck --iso-8601=seconds" 				>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	date -r Linux_amd64_release/archicheck --iso-8601=seconds 		>> Doc/Download.txt
	echo "(end)" 								>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "Dependencies:" 						>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	echo "(start code)" 							>> Doc/Download.txt
	echo "readelf -d archicheck | grep 'NEEDED'" 				>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	readelf -d Linux_amd64_release/archicheck | grep 'NEEDED'		>> Doc/Download.txt
	echo "(end)" 								>> Doc/Download.txt
	echo "" 								>> Doc/Download.txt
	
build Obj/archicheck:
	echo
	echo Make debug build
	gnat make -Xmode=debug -s -Parchicheck.gpr
	# -s : recompile if compiler switches have changed
	# -q : quiet
	# -d : progress

build_release:
	echo
	echo Make release build
	gnat make -Xmode=release -s -Parchicheck.gpr
	# -s : recompile if compiler switches have changed

check: Obj/archicheck
	# depend on the exe, may be either build or build_release, test have to pass with both
	echo
	echo Make Tests
	$(MAKE) --directory=Tests

dashboard: build
	echo
	echo Make dashboard
	sloccount Src Tests |grep "ada=" |  ploticus  -prefab pie 	\
		data=stdin labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		-png -o Doc/sloc.png
	echo
	echo Make Code coverage
	lcov -q -c -d Obj -o Obj/tests.info
	# remove lcov output for Ada lib,and the "false" main :
	lcov -q --remove Obj/tests.info -o Obj/tests.info "/usr/*" "*/Obj/b__archicheck-main.adb"
	genhtml -q Obj/tests.info -o Doc/lcov -t "ArchiCheck tests coverage" -p "/home/lionel/Proj/ArchiCheck"

doc: dashboard
	echo
	echo Make Doc
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

	naturaldocs -q -i Doc -i Src -i Tests 				\
		-s Default archicheck					\
		-xi _darcs						\
		-xi Obj							\
		-o FramedHTML Doc/Generated -p Doc/Natural_Docs
	cp -p Doc/Archicheck_Overview.pdf Doc/Generated
	cp -rp Doc/lcov Doc/Generated
	cp -rp Linux_amd64_release/archicheck Doc/Generated

.PHONY : clean
clean:
	echo
	echo Make clean:
	gnat clean -q -Parchicheck.gpr
	- ${RM} -rf Obj/* Doc/Generated/* Doc/lcov/*
	$(MAKE) --directory=Tests clean

