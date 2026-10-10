<!-- omit from toc -->
# Release procedure

This document is the single reference for:

- selecting a release version;
- preparing and validating a release;
- committing the release state;
- creating the Git tag and GitHub release;
- publishing the release to Alire;
- returning the repository to development mode.

Do not commit, push, tag, create a GitHub release or publish to Alire without
the owner's explicit approval. Do not run `make release` unless explicitly
asked.

## 1. Release prerequisites

A release publishes what the current untagged section of `docs/changelog.md`
describes (the top section, currently labelled "actually means untagged").

Before starting the release procedure, verify that:

- the intended development work is complete;
- the current changelog section is complete and accurate;
- all development changes have been reviewed, committed and pushed;
- the working tree is clean, on the file system and not only through
  `git status`;
- the current branch is the intended release branch;
- a complete local `make all` has succeeded;
- the tests pass in both debug mode (`make build`) and release mode
  (`make build_release`).

There is a continuous integration on this repository
(`.github/workflows/build-test.yml`): it builds acc and runs the test
suites on Linux, macOS and Windows at each push. The latest GitHub
Actions checks must have succeeded on the three platforms before
starting a release.

`alr publish` exposes the crate to the community index tests: Alire builds
the crate and runs its test action on **every platform where the crate is
declared available**. `alire.toml` currently declares no `[available]`
restriction, so the crate is built and tested on Linux, macOS and Windows,
the three platforms covered by the CI: do not publish until the build and
the tests pass on all of them. A crate published while a platform build or
test fails is published broken (this is how acc was once published with
its three platform compilations failing). If a platform cannot be
validated, restrict the `[available]` section of `alire.toml` to the
validated platforms first, and get the owner's approval for that change.

Do not start a release from a dirty or partially generated repository state.

## 2. Select the release version

Do not select the release number by simply removing the development status.
Review the current changelog section and propose the release version according
to SemVer:

- an `[Added]` entry requires a MINOR version bump;
- a backward-compatible `[Changed]` entry requires a MINOR version bump;
- a release containing only backward-compatible bug fixes keeps the PATCH
  bump;
- any incompatible change must be assessed explicitly according to SemVer.

Before changing any file:

1. identify the current development version;
2. review the current changelog entries;
3. propose the release version;
4. explain the SemVer reasoning;
5. request explicit owner confirmation.

Do not change the version before it has been confirmed.

The prerelease suffix must be dash separated:

```text
0.6.1-dev
```

Do not use:

```text
0.6.1.dev
```

The dot-separated form prevents Alire from loading the workspace.

## 3. Set the release version

After the owner confirms the version, update it in:

```text
alire.toml
```

Then run:

```sh
alr update
```

`alr update` regenerates `Crate_Version` in the generated Alire configuration.
Running `alr build` alone does not regenerate it.

Verify that the generated `Crate_Version` matches the confirmed release
version.

Search for other version strings that must be updated, and regenerate the
generated docs so that `docs/cmd_line.md` and the badges in
`docs/generated_img/` match the new exe:

```sh
make doc
```

Do not replace version-independent expressions, regular expressions or
examples unnecessarily.

## 4. Close the release changelog section

In `docs/changelog.md`:

1. verify that the current untagged section accurately describes the release;
2. retitle that section with the confirmed release version;
3. set the release date;
4. preserve the existing changelog structure and categories;
5. keep entries concise and user-oriented;
6. ensure the latest entry appears at the top of its relevant list.

The release notes must reflect the actual content of this changelog section.

## 5. Validate the release locally

Run the complete local validation:

```sh
make all
```

If any part of the build, test, check or documentation generation fails:

1. stop;
2. do not commit, push or tag;
3. fix the issue through the normal development workflow;
4. restart release validation from a clean state.

After a successful full run, verify the release version displayed by:

```sh
obj/acc --version
```

It must match the confirmed release version exactly and must not contain the
`-dev` suffix.

Then run the full release chain, on explicit owner request only:

```sh
make release
```

This builds `acc` in release mode, reruns the tests, regenerates the badges
and `docs/download.md`, and installs the exe in `~/bin`.

Then clean the repository:

```sh
make clean
```

Inspect both Git state and the file system:

```sh
git status
git diff
git diff --staged
```

Verify that:

- no unwanted build or test artefact remains;
- no Git-ignored artefact remains;
- the release version is correct;
- `Crate_Version` was regenerated;
- the changelog section is closed correctly;
- all required generated results, badges, indexes and documentation are
  consistent;
- only intended release changes remain.

## 6. Confirm and commit the release state

Before staging, present a release-preparation report containing:

- the previous development version;
- the proposed release version;
- the SemVer reasoning;
- the changelog entries included;
- the result of `alr update`;
- the version reported by `obj/acc --version`;
- the result of `make all` and `make release`;
- the result of the cleanup;
- the files to be committed;
- the proposed commit message.

Request explicit owner approval before staging, committing or pushing.

After approval, stage the complete release state, including all intended
generated files, then commit and push as described in
`docs/dev/development_workflow.md`.

## 7. Validate the release commit

After pushing, verify that the GitHub Actions checks of that commit have
succeeded on Linux, macOS and Windows.

Do not create or push the release tag while a required check:

- is pending;
- has failed;
- has been cancelled;
- was run against a different commit: the pushed release commit must
  be exactly the locally validated one.

If a correction is required, return to the development workflow, implement and
validate the fix, then restart the release procedure from a clean state.

## 8. Tag and publish the GitHub release

Once the release commit is pushed, prepare:

- the validated release commit;
- the confirmed release version;
- the proposed tag;
- the release notes derived from the changelog.

The tag format is:

```text
x.y.z
```

Request explicit owner approval before creating or publishing the tag.

After approval, create the tag:

```sh
git tag x.y.z
```

Push the tag:

```sh
git push ac x.y.z
```

Then open the
[ArchiCheck GitHub Releases page](https://github.com/LionelDraghi/ArchiCheck/releases)
and create the release from that tag, with release notes reformatted from the
released changelog section. Attach the release exe (the built `obj/acc`,
renamed `acc`) to the GitHub release: `docs/download.md` points to the
releases page.

Before considering the GitHub release complete, verify that:

- the tag name is correct;
- the tag points to the validated release commit;
- the release notes match the changelog;
- the GitHub release is published from the correct tag.

## 9. Publish to Alire

Publish to Alire only after the GitHub release and tag are available.

Before publication, verify that:

- `alire.toml` contains the released version;
- the generated `Crate_Version` contains the same version;
- the Git tag is available remotely;
- the GitHub release is published;
- the local repository is clean;
- the crate builds and its tests pass on every platform where Alire
  will build and test it: Linux, macOS and Windows, since `alire.toml`
  declares no availability restriction; the GitHub Actions checks must
  be green on the three platforms (cf. section 1).

The publication command is:

```sh
alr publish
```

Refer to the
[Alire project publication documentation](https://alire.ada.dev/docs/#publishing-your-projects-in-alire)
when needed.

Before running `alr publish` or submitting any publication change, present
the intended action and request explicit owner approval.

After publication, report:

- the published version;
- the Git tag used;
- the publication or pull-request reference;
- the Alire validation result;
- any requested correction.

## 10. Return to development mode

After the GitHub and Alire publication steps are complete, propose the next
development version. The new version must use the dash-separated `-dev`
suffix, for example:

```text
0.6.1 -> 0.6.2-dev
```

The appropriate next development version depends on the planned work. Request
explicit owner confirmation before changing it.

After confirmation:

1. update the version in `alire.toml`;
2. run `alr update` and verify the regenerated `Crate_Version`;
3. open a new untagged section in `docs/changelog.md`, labelled "actually
   means untagged";
4. run `make doc` so the generated docs match the new development exe;
5. run `make all`;
6. clean and inspect the repository;
7. request approval before staging, committing and pushing.

After approval, commit and push the complete generated development state
according to `docs/dev/development_workflow.md`.

The repository is back in development mode only when:

- the new `-dev` version has been committed;
- the commit has been pushed;
- the working tree and file system are clean.
