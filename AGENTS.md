# Instructions for coding agents

## Commit discipline

- never stage, commit or push without the owner's explicit consent:
  `git add` is as forbidden as `git commit` and `git push`; prepare the
  change, run a full `make all` (build, tools, check, doc), a `make clean`
  to check that there is no remaining unwanted file, and report the result;
  wait for the go-ahead before touching the index or the history
- once the owner gives the go-ahead, run the whole sequence in one go:
  `git add` the whole generated state (test results, badges,
  docs/tests/, docs/dashboard.md, docs/cmd_line.md, docs/fixme.md,
  Tests/tests_count.txt...), commit, and push. Committing in the middle of
  the chain freezes inconsistent artifacts, such as a dashboard still
  referring to the previous version or a stale test badge

## Build

- `make build` builds `acc` in debug mode (`alr build --development`) in obj/
- `make build_release` builds `acc` in release mode (`alr build --release`);
  the tests must pass in both modes
- `make tools` builds the Tools/ sub-project (create_pkg)
- `make release` is the full release chain (release build, tests, badges,
  docs/download.md); do not run it unless explicitly asked

## Test procedure

- tests are bbt scenarios; all suites are described in the flat
  `Tests/[0-9][0-9]_*.md` files, bulk test data stays in the
  `Tests/NN_*` directories and is referenced with its directory prefix
- `Tests/acc` and `Tests/create_pkg` are symlinks made by the test makefile;
  if they are missing, just run the tests makefile, do not copy binaries
- to run a specific suite : `cd Tests && bbt -c -k --yes --exclude @WIP <NN_suite.md>`
- to run all tests       : `cd Tests && make check` (or `make` from the root)
- `--exclude @WIP` skips the work-in-progress tests; never remove them from
  the run to make it pass
- the test results are written by `make check` to
  docs/tests/test_results.md, and the suite files are copied to docs/tests/
  by `make doc`; if you change a suite, both the `Tests/NN_*.md` file and
  docs/tests/ must stay consistent (rerun `cd Tests && make doc`)
- after `make clean`, verify cleanliness on the file system, not only with
  git status: git ignored files (obj/, *.o, *.ali, symlinks, test dirs
  rebuilt by the scenarios such as dir?, src, zip-ada...) remain invisible

## Changing a feature or an error message format

- the test suites in Tests/ are the specification (TDD first)
- error messages and rules file syntax are stabilized by the suites
  01_command_line, 07_rules_files_syntax and 15_precedences_rules: check
  them before changing any message or diagnostic format
- add a line in docs/changelog.md under the current untagged version
  (the top section, currently labelled "actually means untagged"),
  following the Keep a Changelog format with the [Changed]/[Added]/[Fixed]
  keywords
- the changelog is written for users, not for developers: describe only
  features and behavior changes visible from outside (new rule, new option,
  modified output or exit code); do not mention internal changes (build,
  refactoring, test tooling, dependencies); keep [Fixed] entries very
  short (one line, what was wrong from the user's point of view)
- `Fixme:` comments in src/, docs/ and Tests/ are indexed in docs/fixme.md
  by `make doc`; add one rather than leaving an untracked TODO in the code
- examples and generated docs (cmd_line.md, dashboard.md) must not depend
  on the machine or on the locale: no hard version number (use a regexp
  or a fixed behavior), and set the environment variable LC_ALL to C
  when a tool message is checked

## Alire and version numbering

- the project is the Alire crate `acc` (alire.toml); exe is `acc`,
  and a plain `acc` invocation in the tests means `./obj/acc`
- the pre-release suffix must be dash separated (0.6.1-dev), not dot
  (0.6.1.dev), otherwise alr cannot load the workspace
- after a version change in alire.toml, `alr update` regenerates
  Crate_Version; `alr build` alone does not
- after changing the version or the command line help, regenerate the
  generated docs (`make doc`) so that docs/cmd_line.md, the version badge
  in docs/generated_img/ and docs/dashboard.md match the new exe

## Pointers

- to understand acc: docs/index.md, docs/rules.md, docs/acc_concepts.md
- design: docs/design_overview.md, docs/languages_concepts.md
- to do list: docs/limitations.md, docs/fixme.md, notes in docs/notes.txt
- the Tools/ sub-project (create_pkg) has its own makefile; testrec is
  retired from the default chain (replaced by bbt) but its sources and
  self-test targets remain in Tools/, runnable manually
- acc is tested with bbt (../bbt); its grammar is the reference
