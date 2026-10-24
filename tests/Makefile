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

all: check

.PHONY : help
help:
	@ echo "Usage: make [target]"
	@ echo ""
	@ echo "Targets:"
	@ echo "  all   : check (default when no target given)"
	@ echo "  check : refresh the acc and create_pkg links, then run the"
	@ echo "          [0-9][0-9]_*.md bbt suites, and write the results"
	@ echo "          index and badge in ../docs/tests_results/"
	@ echo "  clean : remove the links and the test artifacts"

.PHONY : clean

# The suites live in docs/features, next to the golden files and images
# they embed; this Makefile is only the place of execution (acc and
# create_pkg links, runtime artifacts). Bulk test data stays in the
# docs/features/NN_* directories and is referenced from the .md files
# with its directory prefix.

check: ../obj/acc$(EXE_SUFFIX) ../Tools/create_pkg$(EXE_SUFFIX)
	@ - $(REFRESH) ../obj/acc$(EXE_SUFFIX) acc$(EXE_SUFFIX)
	@ - $(REFRESH) ../Tools/create_pkg$(EXE_SUFFIX) create_pkg$(EXE_SUFFIX)
	@ bbt --cleanup=full -k --yes --exclude @WIP --index ../docs/tests_results/test_results.md --generate_badge badge.url ../docs/features/[0-9][0-9]_*.md
	@ # The bbt run stores the scenario paths as given on the command
	@ # line, relative to tests/: reanchored to the index location, the
	@ # links resolve from docs/tests_results/test_results.md.
	@ sed 's|](\.\./docs/features/|](\.\./features/|g' ../docs/tests_results/test_results.md > ../docs/tests_results/test_results.tmp
	@ mv ../docs/tests_results/test_results.tmp ../docs/tests_results/test_results.md
	@ # The tests badge of the README: the bbt run writes the shields.io
	@ # URL of the badge, and the badge is fetched from it, published with
	@ # the repository through docs/tests_results/badge.svg. Like every
	@ # evidence, the committed file must match the fresh run.
	@ curl -s -o ../docs/tests_results/badge.svg "$$(cat badge.url)"

# The tests directory holds nothing tracked but the Makefile: clean
# removes every runtime artifact left by the bbt suites (extracted
# third-party sources, create_pkg dirs, rules files, links...)
clean:
	@ - ${RM} acc$(EXE_SUFFIX) create_pkg$(EXE_SUFFIX)
	@ find . -mindepth 1 -maxdepth 1 ! -name Makefile -exec ${RM} -rf {} +
	@ find . -name gmon.out -delete
