# Instructions for coding agents

This file is the entry point for coding agents working on ArchiCheck, the
`acc` software architecture compliance verifier.

## Non-negotiable rules

- Never stage, commit, push, tag, create a release or publish to Alire without
  the owner's explicit consent.
- `git add`, `git commit` and `git push` are forbidden until the owner gives
  the corresponding go-ahead.
- If a build, test, documentation, cleanup or publication step fails, stop and
  report the failure.
- Do not include unrelated changes, temporary files or unwanted test artefacts.
- Check repository cleanliness on the file system, not only with `git status`:
  Git-ignored files (obj/, *.o, *.ali, the Tests/acc and Tests/create_pkg
  symlinks, test directories rebuilt by the scenarios such as dir?, src,
  zip-ada...) remain invisible to Git.
- If `make clean` leaves an unwanted artefact behind, update the clean target
  so that this type of artefact is removed automatically in the future.

## Choose the right procedure

- Development work, bug fixes, doc updates, tests, cleanup, review: use
  `docs/dev/development_workflow.md`
- Release versioning, tagging, GitHub release, Alire publication: use
  `docs/dev/release_procedure.md`

## Build pointers

- `make build` builds `acc` in debug mode (`alr build --development`) in obj/
- `make build_release` builds `acc` in release mode (`alr build --release`);
  the tests must pass in both modes
- `make tools` builds the Tools/ sub-project (create_pkg)
- `make all` runs build, tools, check and doc
- `make check` runs the bbt test suites and writes docs/tests/test_results.md
- `make doc` regenerates the generated docs (fixme index, tests doc,
  cmd_line.md, badges)
- `make release` is the full release chain (release build, tests, badges,
  docs/download.md, install in ~/bin); do not run it unless explicitly asked
- `make clean` removes the build and test artefacts
- The GitHub Actions workflows (`.github/workflows/`) build and test
  acc on Linux, macOS and Windows at each push, and upload an AppImage
  to the rolling `latest` GitHub release. The tests results and badges
  are not published by the CI: they are part of the repository state.

## Task triage

When the user asks "what should we do now?" or "what is the priority right
now?", check the project backlog and tracked work before proposing new work:

- docs/todo.md and docs/limitations.md for the project to-do list and known
  bugs;
- docs/fixme.md for actionable fixes and cleanup items;
- notes in docs/notes.txt;
- docs/dev/design_discussions.md for decisions that constrain the next step.

Use these sources to ground the answer in the repository's existing work. Do
not invent a new task before checking whether the need is already tracked
elsewhere.

## Useful references

- to understand acc: docs/index.md, docs/rules.md, docs/acc_concepts.md
- design: docs/design_overview.md, docs/languages_concepts.md,
  docs/dev/design_discussions.md
- the Tools/ sub-project (create_pkg) has its own makefile
- acc is tested with bbt (../bbt); its grammar is the reference
