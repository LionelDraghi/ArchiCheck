# Instructions for coding agents

This file is the entry point for coding agents working on ArchiCheck, the
`acc` software architecture compliance verifier.

The shared rules for Lionel's Ada projects live in the [reference
repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.template.md)
and apply here in full:
non-negotiable git rules, procedure choice, task triage, reference
policy, skills, and the definition of done. This file keeps only what
is specific to ArchiCheck.

## Build pointers

The shared build pointers apply ([reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.template.md)); `make help` is the documentation of record. ArchiCheck specifics:

- Naming deviation: the release-mode build is `make build_release`;
  `make release` is the full release chain (release build, tests,
  `docs/download.md`, install in `~/bin`) — do not run it unless
  explicitly asked.
- `make tools` builds the Tools/ sub-project (create_pkg).
- `make check` runs the bbt test suites in tests/ and writes
  `docs/tests_results/test_results.md`; the tests must pass in both validation
  and release modes.
- `make doc` regenerates the generated docs (fixme index, tests doc,
  `cmd_line.md`, badges).
- The GitHub Actions workflows (`.github/workflows/`) build and test
  acc on Linux, macOS and Windows at each push, and upload an AppImage
  to the rolling `latest` GitHub release. The tests results and badges
  are not published by the CI: they are part of the repository state.
- If `make clean` leaves an unwanted artefact behind, update the clean target
  so that this type of artefact is removed automatically in the future.

## Conditional readings

Read a document only when the task requires it; do not open them all up front.

- To understand `acc` and its concepts: `docs/index.md`, `docs/rules.md`,
  `docs/acc_concepts.md`
- To review or challenge the design: `docs/design_overview.md`,
  `docs/languages_concepts.md`, `docs/dev/design_discussions.md`
- To choose the next task or find already-tracked work: `docs/TDL.md`,
  `docs/limitations.md`, `docs/dev/fixme_index.md`, `docs/notes.txt`
- To prepare or run a release: the [common release procedure on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md), with the ArchiCheck parameters and additions in `docs/dev/release_procedure.md`
- The Tools/ sub-project (create_pkg) has its own makefile.
- `acc` is tested with bbt (`../bbt`); its grammar is the reference.

