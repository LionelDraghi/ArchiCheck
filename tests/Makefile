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
	@ echo "          [0-9][0-9]_*.md bbt suites"
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
	@ bbt --cleanup=full -k --yes --exclude @WIP ../docs/features/[0-9][0-9]_*.md

# The tests directory holds nothing tracked but the Makefile: clean
# removes every runtime artifact left by the bbt suites (extracted
# third-party sources, create_pkg dirs, rules files, links...)
clean:
	@ - ${RM} acc$(EXE_SUFFIX) create_pkg$(EXE_SUFFIX)
	@ find . -mindepth 1 -maxdepth 1 ! -name Makefile -exec ${RM} -rf {} +
	@ find . -name gmon.out -delete
