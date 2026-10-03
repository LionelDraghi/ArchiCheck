all: check doc

.PHONY : clean

# All test suites are described in the flat [0-9][0-9]_*.md bbt files,
# and run from this directory by a single bbt invocation.
# Bulk test data stays in the Tests/NN_* directories and is referenced
# from the .md files with its directory prefix.

check: ../obj/acc ../Tools/create_pkg
	@ - ln -s ../obj/acc .
	@ - ln -s ../Tools/create_pkg .
	@ bbt -c -k --yes --exclude @WIP -o ../docs/tests/test_results.md [0-9][0-9]_*.md

doc:
	@ for md in [0-9][0-9]_*.md; do cp $$md ../docs/tests/$$md; done

clean:
	@ - ${RM} acc create_pkg template.ac test_results.md
	@ - ${RM} *.o *.ali sub-test
	@ - ${RM} 13_Ada_Units/src/*.o 13_Ada_Units/src/*.ali 13_Ada_Units/src/sub-test
	@ - ${RM} -rf dir? dir3? src src1 src2 include zip-ada gtkada-master \
	           adactl-1.19r10 batik-1.9 spring-petclinic-master \
	           spring-framework-petclinic-master
	@ find . -name gmon.out -delete
