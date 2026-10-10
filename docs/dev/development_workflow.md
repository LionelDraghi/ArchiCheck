<!-- omit from toc -->
# Development workflow

This document describes the workflow for normal development tasks on ArchiCheck.

Its purpose is to keep changes easy to review and ensure that commits contain
a complete, consistent and validated project state.

## 1. Development loop

Work on the source files without staging, committing or pushing.

The project is the Alire crate `acc` (alire.toml); the executable is `acc`, and
a plain `acc` invocation in the tests means `./obj/acc`.

The normal development loop is:

```sh
make check
```

To run a single suite:

```sh
cd Tests && bbt -c -k --yes --exclude @WIP <NN_suite.md>
```

To run all tests:

```sh
cd Tests && make check
```

All test suites are described in the flat `Tests/[0-9][0-9]_*.md` bbt files,
and run from this directory by a single bbt invocation. Bulk test data stays
in the `docs/tests/<suite>` directories, next to the suite pages, and is
referenced from the `.md` files with its `../docs/tests/<suite>/` prefix.

When running tests:

- `--exclude @WIP` skips the work-in-progress tests; never remove them from
  the run to make it pass;
- `Tests/acc` and `Tests/create_pkg` are symlinks made by the test makefile;
  if they are missing, just run the tests makefile, do not copy binaries.

Run the relevant tests throughout development. Fix failures before continuing.

## 2. Feature and error-message changes

The test suites in `Tests/` are the specification. Follow a TDD-first approach
when changing a feature or an error-message format.

Error messages and rules file syntax are stabilized by the suites
`01_command_line`, `07_rules_files_syntax` and `15_precedences_rules`: check
them before changing any message or diagnostic format.

For every functional change:

1. update the relevant suite;
2. implement the change;
3. update or add the relevant tests;
4. update the documentation, examples and help text where applicable;
5. add a line in `docs/changelog.md` under the current untagged version (the
   top section, currently labelled "actually means untagged").

Changelog entries must follow the Keep a Changelog format with the
[Changed]/[Added]/[Fixed] keywords, and:

- the changelog is written for users, not for developers: describe only
  features and behavior changes visible from outside (new rule, new option,
  modified output or exit code);
- do not mention internal changes (build, refactoring, test tooling,
  dependencies);
- keep [Fixed] entries very short (one line, what was wrong from the user's
  point of view).

Examples and generated docs (cmd_line.md) must not depend on the
machine or on the locale: no hard version number (use a regexp or a fixed
behavior), and set the environment variable LC_ALL to C when a tool message is
checked.

Fixme comments in src/, docs/ and Tests/ are indexed in `docs/fixme.md` by
`make doc`; add one rather than leaving an untracked TODO in the code.

## 3. Generated documentation

The test results are written by `make check` to `docs/tests/test_results.md`,
and the suite files are copied to `docs/tests/` by `make doc`. If a suite is
changed, both the `Tests/NN_*.md` file and `docs/tests/` must stay consistent:

```sh
cd Tests && make doc
```

After changing the version or the command line help, regenerate the generated
docs so that `docs/cmd_line.md` and the badges in `docs/generated_img/`
match the new exe:

```sh
make doc
```

## 4. Review implementation completeness

When the relevant tests pass, verify that all impacted project elements are up
to date, including where applicable:

- source files;
- test suites;
- documentation;
- changelog;
- Fixme comments and index;
- generated documentation (cmd_line.md, badges, docs/tests/).

Review the modified sources critically:

- is the implementation complete?
- is obsolete code left behind?
- are comments and documentation still accurate?
- are there unrelated changes?

Do not stage the changes yet. Modified, staged and untracked files must remain
available for review in the editor and through Git tools.

## 5. Clean before owner review

When the development loop is green, run:

```sh
make clean
```

Then inspect the working tree:

```sh
git status
git diff
git diff --staged
```

The repository may contain useful uncommitted changes, but it must not contain
unwanted test artefacts, generated temporary outputs, debug traces, editor
files, compiler outputs or unrelated files.

Check the file system directly, not only with `git status`. Git-ignored files
(obj/, *.o, *.ali, the Tests/acc and Tests/create_pkg symlinks, test dirs
rebuilt by the scenarios such as dir?, src, zip-ada...) remain invisible to
Git.

Files created by `When I run` steps, such as `template.ac` output by
`acc -ct` or the `.ali` files produced by the gcc compilation check, are not
tracked by the bbt cleanup, which only tracks files created by `Given` steps.

If `make clean` leaves an unwanted artefact behind, update the clean target so
that this type of artefact is removed automatically in the future, then run
`make clean` again and repeat the file-system and Git review.

## 6. Owner review and approval

The owner reviews the useful modified, staged and untracked files before any
new staging, commit or push.

Present a concise report containing:

- what was changed;
- which tests were run;
- the result of those tests;
- the documentation and changelog updates;
- any generated files that changed;
- the files currently modified, staged or untracked;
- any remaining warning, uncertainty or proposed follow-up.

Then ask explicitly for approval before touching the index or the history.
Wait for the go-ahead before running any `git add`, `git commit` or
`git push`.

## 7. Final validation after approval

After the owner gives the go-ahead, run the complete validation:

```sh
make all
```

This covers the complete build, tools, checks and documentation generation.
If any step fails:

1. stop;
2. do not stage, commit or push;
3. report the failure.

If `make all` succeeds, run:

```sh
make clean
```

Then inspect the working tree and the file system again:

```sh
git status
git diff
git diff --staged
```

Verify that:

- no unwanted test or build artefact remains;
- no Git-ignored artefact remains on the file system;
- all intended generated results, badges and indexes are present;
- cleanup has not removed a file that must be committed;
- the remaining changes are exactly those intended for the commit.

If an unwanted artefact remains, update the cleanup rules and repeat the
necessary validation.

## 8. Stage and review the complete generated state

Once the final validation and cleanup are successful, stage the complete
approved state in one go:

```sh
git add <approved files>
```

The staged state must include the whole generated state: test results,
badges, docs/tests/, docs/cmd_line.md, docs/fixme.md...

Do not commit in the middle of the generation chain: committing after the
tests alone can freeze inconsistent artefacts, such as a badge still
referring to the previous version or to the previous test run.

Review the exact staged content:

```sh
git status
git diff --staged
```

If the staged state differs from what the owner approved, stop and report the
difference before committing.

## 9. Commit, rebase and push

Create a focused commit describing the approved change:

```sh
git commit
```

Before pushing, synchronise with the remote branch:

```sh
git pull --rebase ac master
```

If a rebase conflict affects generated test results, badges or similar
generated files, keep the local version produced by the latest complete run.

If a conflict affects source code, documentation or another non-generated
file, stop and ask the owner to arbitrate rather than resolving the conflict
silently.

After a successful rebase, push:

```sh
git push ac master
```

Do not amend commits, force-push or rewrite published history without
explicit owner approval.

Report:

- the commit created;
- the branch pushed;
- the rebase result;
- the push result.

## 10. Design discussions and pending work

Record design subjects in:

```text
docs/dev/design_discussions.md
```

Use one entry per subject, with one of these statuses: under discussion, or
arbitrated. An arbitrated entry must state the decision, the rejected
alternatives and why they were rejected. A reference from the source code to
a design-discussion entry is welcome when it helps understanding.

Record an undecided feature idea or a pending work item in:

```text
docs/todo.md
```
