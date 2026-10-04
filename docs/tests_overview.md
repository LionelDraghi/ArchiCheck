<!-- omit from toc -->
# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution results in a global count of passed/failed tests, and in a text output in Markdown format, integrated in this documentation.

Tests are at exe level, with no unit testing at this stage. 

Test suites are defined in the flat `Tests/[0-9][0-9]_*.md` files, using the [bbt](https://lionel.draghi.free.fr/bbt) Gherkin-like grammar (`Feature`, `Scenario`, `Given`, `When`, `Then`). 

Bulk test data (sources, rules files, third-party software extracts) stays in the `Tests/NN_*` directories, and is referenced from the flat files with its directory prefix. All test execution is driven by a single central `Tests/Makefile`.

A test typically documents (order may vary):

1. When running _this_ command, 
2. with _this_ rules file,
3. and _those_ sources files, or _those_ dependencies between sources (details are not always printed),
4. I should have _that_ result (on standard output, but also on error output, and returned code)

Execution is typically :

1. the `Tests/acc` and `Tests/create_pkg` symlinks are created (or refreshed) toward the built binaries; never copy binaries in the tests directory;
2. then a single `bbt` invocation runs all the suites, with the `--exclude @WIP` option skipping the work-in-progress scenarios;
3. bbt writes the consolidated results to `docs/tests/test_results.md`;
4. the `doc` target copies the flat suite files to `docs/tests/`, where they are automatically taken into account by the documentation build, and counts the passed and failed tests in `Tests/tests_count.txt`.

So, to run a specific suite : `cd Tests && bbt -c -k --yes --exclude @WIP <NN_suite.md>`, and to run them all : `cd Tests && make check`.
