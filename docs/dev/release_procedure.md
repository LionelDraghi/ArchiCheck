<!-- omit from toc -->
# Release procedure

The release procedure is common to all Lionel's Ada projects and lives
in [the reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md).
Read both documents together; on any conflict, this file wins.

This file gives the parameter values and the ArchiCheck-specific
additions; all other steps are described in the common procedure.

## Parameters

- Changelog: `docs/changelog.md`; the development section is the
  untagged top section (currently labelled "actually means untagged");
  its categories are `[Added]`, `[Changed]`, ...
- Complete local validation: `make all` (build, tools, check, doc).
  Release chain: `make release` — on explicit owner request only.
- Version check: `obj/acc --version`.
- Regenerated configuration: the generated Alire configuration.
- Clean command: `make clean`.
- Remote: `ac`.
- GitHub releases page:
  [LionelDraghi/ArchiCheck releases](https://github.com/LionelDraghi/ArchiCheck/releases).
- Required GitHub Actions checks
  (`.github/workflows/build-test.yml`): Linux, macOS and Windows.
- Alire platform availability: `alire.toml` declares no `[available]`
  restriction; the crate is built and tested on Linux, macOS and
  Windows, the three platforms covered by the CI.

## ArchiCheck-specific additions

- Step 1: the tests must pass in both debug mode (`make build`) and
  release mode (`make build_release`).
- Step 3: regenerate the generated docs so that `docs/cmd_line.md`
  and the badges in `docs/generated_img/` match the new exe:
  `make doc`.
- Step 5: do not run `make release` unless explicitly asked; it builds
  `acc` in release mode, reruns the tests, regenerates the badges and
  `docs/download.md`, and installs the exe in `~/bin`.
- Step 8: attach the release exe to the GitHub release (the built
  `obj/acc`, renamed `acc`): `docs/download.md` points to the releases
  page.
- Step 9: during development, `bbt`, `opentoken` and `list_image` are
  pinned to sibling directories; before the release commit, remove the
  `[[pins]]` entries and rely on the index dependencies. If a pin
  carries a fix not yet available in the index, release that crate
  first.
- Step 10: after changing the version, run `make doc` so the
  generated docs match the new development exe, then `make all`.
