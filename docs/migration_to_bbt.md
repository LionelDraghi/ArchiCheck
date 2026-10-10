<!-- omit from toc -->
# Migration to bbt

This page gives the objective balance sheet of the test suites migration:
line counts of Makefiles, input files and expected files, before and after.

## The general intent

The migration changed the general intent itself, as stated in
[tests_overview.md](tests_overview.md):

- before: *The global intent is to have tests documenting the software
  behavior* — the tests were scripts that had to be written and maintained,
  and the documentation was an output of the test run;
- now: *The global intent is to have just documentation of the behavior,
  no test scripts, and to have bbt run that documentation to check that
  what is documented is what is implemented* — the documentation is the
  input, and nothing else has to be written.

## Tooling

Before the migration, the suites were driven by **testrec**, a tool
developed inside the project; the test descriptions and expected outputs
were produced by the testrec runs, and the orchestration lived in one
Makefile per suite directory, plus a central `Tests/Makefile` chaining
them and compiling the global results. testrec is now completely removed
from the repository, and replaced by [bbt](https://github.com/LionelDraghi/bbt),
which executes the suites directly. The tools themselves (features,
performances, code size) are not compared in this document.

The decision is recorded in
[dev/design_discussions.md](dev/design_discussions.md) (D2. Test tooling:
bbt replaces testrec).

## Balance sheet

Measured on the `Tests/` and `docs/tests/` trees:

- **before**: commit `a93ec9c` (April 2024), the last state where every
  suite was driven by testrec;
- **after**: the current tree.

| Indicator              | testrec era                                                     | today (bbt)                                                                                  |
|------------------------|-----------------------------------------------------------------|----------------------------------------------------------------------------------------------|
| Test orchestration     | 21 per-suite Makefiles + 1 central Makefile (117 lines) chaining them and compiling the global results: 3125 lines in total | 1 central Makefile, 27 lines                                                                 |
| Test documentation    | generated at run time by `testrec -o`, then committed in `docs/tests/`: 21 pages, 2763 lines, an **output** | written once as the flat suites `Tests/[0-9][0-9]_*.md`: 22 files, 2507 lines, given as **input** to bbt, then copied to `docs/tests/` by the `doc` target |
| Test index and results | compiled by the central Makefile, grepping the generated pages: `Tests/tests_count.txt` and `docs/tests/tests_status.md` (108 lines) | written natively by bbt: `docs/tests/test_results.md` (1147 lines), the single source also used by the release chain |
| Expected outputs      | 80 files, 20290 lines                                           | 40 files, 20175 lines, the rest inline in the suites                                          |
| Input rules files      | 19 files, 62 lines                                              | 20 files, 43 lines, the rest inline in the suites                                             |
| Stored test sources   | 36 files, 225 lines                                             | 30 files, 193 lines, the rest inline in the suites or created by `create_pkg` in the steps    |
| Scenarios             | 120 tests OK, 0 failed, 2 empty, as compiled in the old `tests_status.md` | 103 scenarios run, plus one work-in-progress scenario excluded by `--exclude @WIP`            |

Reading notes:

- what corresponds to today's scenarios was, before, spread over the
  Makefiles (the actual test scripts), the input and expected files, and
  the generated documentation; counting inputs and committed outputs on
  both sides, the totals are nearly identical (26 573 lines before,
  26 599 lines after), but the distribution changed completely:
  - before: 23 702 lines of inputs, of which 3 125 lines of test
    scripts, plus 2 871 lines of generated documentation on the output
    side;
  - after: 22 945 lines of inputs, of which 27 lines of Makefile, plus
    3 654 lines on the output side (the doc copies and the consolidated
    results index);
- the two large figures (expected outputs, stored sources) barely moved:
  they are dominated by the third-party extracts (GtkAda, AdaControl,
  Zip-Ada, Batik, Spring PetClinic) that were moved unchanged into the
  per-suite data directories, now in `docs/tests/<suite>/`;
- the global results compilation exists in both eras: before, it was
  hand-written shell in the central Makefile, grepping the generated
  pages to produce `tests_count.txt` and `tests_status.md`; today, bbt
  writes the consolidated index natively (`test_results.md`), and the
  release chain derives its counts from its summary table. The old index
  page, `docs/tests/tests_status.md`, was later removed, its content
  being fully covered by `test_results.md`.

## Measurement method

Before: `git ls-tree -r a93ec9c Tests` and
`git ls-tree -r a93ec9c docs/tests`, then `wc -l` per file, summed per
category. After: the same counts on the current tree, with the bulk test
data in `docs/tests/<suite>`, the flat suites in `Tests/[0-9][0-9]_*.md` and
their copies in `docs/tests/`.

See [tests_overview.md](tests_overview.md) for the current organization
and the run commands.
