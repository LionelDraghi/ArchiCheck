<!-- omit from toc -->
# Design discussions

- D1. Rules file parsing: dropping OpenToken
- D2. Test tooling: bbt replaces testrec
- D3. Suite 13 source sanity check

This document holds the design discussions: each entry records a significant
design subject, with its status - under discussion, or arbitrated - and, once
arbitrated, the decision, the alternatives that were rejected and why. When
the discussion happened elsewhere (a commit, a changelog entry, a GitHub
discussion), the entry references it and adds only the complements needed to
understand the subject as it stands.

An arbitrated entry may be superseded by a later entry; it is then updated
with a reference to its replacement.

| Subject                                                            | Status                                       | References                                     |
|--------------------------------------------------------------------|----------------------------------------------|------------------------------------------------|
| D1. Rules file parsing: dropping OpenToken | Arbitrated (2026), implementation in progress | [changelog.md](../changelog.md), `src/acc-rules-parser.ads` |
| D2. Test tooling: bbt replaces testrec | Arbitrated (2026-10), implemented            | `Tests/`                            |
| D3. Suite 13 source sanity check | Arbitrated (2026-10), implemented            | [13_ada_units.md](../tests/13_ada_units.md)    |

The table is sorted by status: the entries under discussion first.

## D1. Rules file parsing: dropping OpenToken

Status: arbitrated, implementation in progress

The rules file grammar was historically implemented with the OpenToken
library (cf. [design_overview.md](../design_overview.md)). The decision,
recorded in the [changelog](../changelog.md) under the current untagged
version, is to drop OpenToken for the rules file and return to a hand written
lexer.

The migration is not complete: `src/acc-rules-parser.adb`
still uses OpenToken for tokenizing ("Short description, but big OpenToken
mess inside"), and [design_overview.md](../design_overview.md) still
documents the OpenToken-based implementation.

OpenToken remains the reference for Ada and Java source processing
(`src/acc-lang-ada_processor.adb`,
`src/acc-lang-java_processor.adb`); the
decision only concerns the rules file.

## D2. Test tooling: bbt replaces testrec

Status: arbitrated (October 2026), implemented

The test suites are bbt scenarios (cf. [development_workflow.md](
development_workflow.md) and [migration_to_bbt.md](../migration_to_bbt.md)),
and bbt (../bbt) is the reference tool: its grammar is the reference for the
suite files.

The previous home-made tool, testrec, was rejected for the default chain,
then completely removed from the repository (sources, project file and
self-test data): the suites are plain Markdown files, run by a single bbt
invocation from Tests/, with bulk test data in the docs/tests/<suite>
directories.

Milestones: tests migration to bbt and central Tests/Makefile, bbt suite
fixed to actual bbt grammar and fully passing, testrec retired from the
build chain (commit `c894f15`), modernization with bbt 0.4 features (commit
`b4086e4`).

## D3. Suite 13 source sanity check

Status: arbitrated (October 2026), implemented

The purpose of the compilation step in the
[13_ada_units](../tests/13_ada_units.md) suite is only to verify that the
test sources compile; the tested behavior is the dependency list produced by
acc, not the build.

The decision is to use `gcc -c -gnatc` on all the bodies of the sub-test
closure, with the sources checked semantically but no code generated:

- the GNAT subunits of sub-test cannot generate code when compiled
  standalone, so `-gnatc` is required to check them;
- the deliberately invalid or misnamed test data (`body.ads`, `enum_io.ads`)
  is excluded from the check, as `gnat make` was already doing.

Rejected alternatives:

- `gnat make` (the previous solution): the `gnat` and `gnatmake` drivers are
  only available under version-suffixed names on some distributions (for
  example `gnatmake-16`), which made the suite machine-dependent;
- `gprbuild`: it would have required adding a project file to the test data,
  for no additional checking value.

Note: bbt does not run commands through a shell, so the sources are listed
explicitly in the suite; a wildcard would be passed verbatim as an argument.
