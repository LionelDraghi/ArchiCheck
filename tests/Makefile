# On Windows the executables carry an .exe suffix, and ln -s silently
# falls back on a timestamp-preserving copy: an explicit cp is more
# honest, and sets a fresh mtime at each refresh, so that the staleness
# is observable
ifeq ($(OS), Windows_NT)
	EXE_SUFFIX := .exe
	REFRESH := cp -f
else
	EXE_SUFFIX :=
	REFRESH := ln -fs
endif

all: check doc

.PHONY : help
help:
	@ echo "Usage: make [target]"
	@ echo ""
	@ echo "Targets:"
	@ echo "  all   : check and doc (default when no target given)"
	@ echo "  check : refresh the acc and create_pkg links, then run the"
	@ echo "          [0-9][0-9]_*.md bbt suites"
	@ echo "  doc   : copy the suite files to ../docs/tests_results/"
	@ echo "  clean : remove the links and the test artifacts"

.PHONY : clean

# All test suites are described in the flat [0-9][0-9]_*.md bbt files,
# and run from this directory by a single bbt invocation.
# Bulk test data stays in the tests/NN_* directories and is referenced
# from the .md files with its directory prefix.

check: ../obj/acc$(EXE_SUFFIX) ../Tools/create_pkg$(EXE_SUFFIX)
	@ - $(REFRESH) ../obj/acc$(EXE_SUFFIX) acc$(EXE_SUFFIX)
	@ - $(REFRESH) ../Tools/create_pkg$(EXE_SUFFIX) create_pkg$(EXE_SUFFIX)
	@ bbt --cleanup=full -k --yes --exclude @WIP --index ../docs/tests_results/test_results.md [0-9][0-9]_*.md

doc:
	@ for md in [0-9][0-9]_*.md; do cp $$md ../docs/tests_results/$$md; done

clean:
	@ - ${RM} acc$(EXE_SUFFIX) create_pkg$(EXE_SUFFIX) template.ac test_results.md
#	@ - ${RM} *.o *.ali sub-test src.adb test?.ac rules*
#	@ - ${RM} -rf dir? dir3? dir?? src src1 src2 include zip-ada gtkada-master \
#	           adactl-1.19r10 batik-1.9 spring-petclinic-master \
#	           spring-framework-petclinic-master
	@ find . -name gmon.out -delete
