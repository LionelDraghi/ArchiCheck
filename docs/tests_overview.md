<!-- omit from toc -->
# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution result in a global count of passed/failed/empty tests, and in a text output in Markdown format, integrated in this documentation.

Tests are at exe level, with no unit testing at this stage. To have tests in Makefile behaving like unit test, I created a utility, called `testrec` to record tests execution. 
This utility is build before test execution, and record in a local testrec.md file : comments, test suite start, test start, assertion result, etc.
This is how the test documentation is created, and how I can compile a global tests results (like if it was a single Ada unit test), despite multiple Makefiles and executions.

Tests are defined in Tests subdirs, in (hopefully) coherent subsets.
All tests directories are structured the same way, with Makefiles also structured the same way, and the same naming convention.

Most of the Makefile code aim at documentation production. 

The Makefile himself follow a pattern.
A Test typically documents (order may vary) :

1. When running _this_ command, 
2. with _this_ rules file,
3. and _those_ sources files, or _those_ dependencies between sources (details are not always printed),
4. I should have _that_ result (on standard output, but also on error output, and returned code)

Comments are created in Makefile thanks to `testrec cmt`. They are put in the testrec.md file, not on standard output. 
On standard output, only a line of the form  

> _Test suite name / test name [Successful]_  

is put, to keep Makefile execution clean.

Execution is typically :

1. at the beginning of the Makefile, `testrec create` is called to start a new test suite.
   NB : this is a convention, each Makefile run a single test suite.
2. Then, for each test, `testrec start` is called to start the test (and name it).
3. During the test, `testrec assert` is called at least once, generally to check that the output of archicheck execution is equal to the expected output.
4. Finally, `testrec end` is called. `testrec` will then output the test result on standard output.

The test result may be : 

- _Successful_, if all Assert are verified,
- _Failed_, if at least one is not,
- and _Empty_, if no Assert is called between test start and test end. This is useful when starting to write a test in the Makefile before code exists : it wouldn't be fair to have those test "Failed".

After all tests execution, `testrec clean` is called to remove the hidden file that stores intermediate results and state, and the `testrec.md` file is moved in the docs directory under a name matching the test suite name, where it will be automatically taken into account.