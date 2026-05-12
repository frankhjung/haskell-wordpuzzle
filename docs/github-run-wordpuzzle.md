# Run WordPuzzle on GitHub — Specification

## Purpose

Specify the behaviour and implementation details for running the pre-built
WordPuzzle native executable using GitHub Actions. The document describes the
current GitHub workflow implementation, required inputs, artefact handling,
security considerations and recommended operational practices.

## Scope

This specification applies to the GitHub Actions workflows in this repository
(`.github/workflows/cicd.yml` and `.github/workflows/run-wordpuzzle.yml`). It
assumes the build workflow produces a release artefact named
`wordpuzzle-release.tar.gz` that contains the `wordpuzzle` executable and the
`dictionary` file.

## Definitions

- artefact: packaged output (tarball) containing the executable and dictionary.
- TARGET_TAG: release tag (for example `v1.0.0`) that identifies a GitHub
  release containing the artefact.

## Requirements

1. The build workflow shall execute for push events on any branch and for tag
   pushes.
2. On tag pushes that match `^v`, the build workflow shall produce and attach a
   release artefact named `wordpuzzle-release.tar.gz` containing `wordpuzzle`
   and `dictionary` to a GitHub Release.
3. The `run-wordpuzzle` workflow shall be manually triggerable
   (`workflow_dispatch`).
4. The `run-wordpuzzle` job shall not recompile the project; it must download
   the release artefact from the corresponding GitHub Release and execute the
   contained binary.
5. The `run-wordpuzzle` job shall run on a minimal runner image and install only
   necessary utilities to fetch and extract the artefact (for example, `gh`,
   `curl`, `tar`).
6. The `run-wordpuzzle` workflow shall accept inputs `SIZE`, `LETTERS`, and
   `REPEATS`, normalise them, and pass them to the executable.

## Current Implementation (mapping to repository files)

- `.github/workflows/cicd.yml`
  - Builds, tests and (on tag pushes matching `v*`) packages the executable and
    dictionary into `wordpuzzle-release.tar.gz`.
  - Uses `softprops/action-gh-release` to upload the artefact to a GitHub
    Release when a tag is created.

- `.github/workflows/run-wordpuzzle.yml`
  - Trigger: `workflow_dispatch` with inputs `SIZE` (string), `LETTERS`
    (string), and `REPEATS` (boolean).
  - Runs-on: `ubuntu-latest`.
  - Steps:
    - Download the release artefact using `gh release download --pattern
      "wordpuzzle-release.tar.gz"` (requires `GITHUB_TOKEN` available to the
      job), extract it and expose `EXE_PATH` to the job environment.
    - Normalise inputs (`SIZE_INT`, lowercase `LETTERS`) and construct the
      argument list.
    - Execute `./wordpuzzle --dictionary=./dictionary` with the constructed
      arguments.

## Security Considerations

- `GITHUB_TOKEN` (or `gh` authentication) is used to download release assets;
  ensure token permissions are scoped appropriately and that release assets are
  not publicly modifiable.
- Consider validating checksums or signatures of the downloaded artefact prior
  to execution if artefact integrity is a concern.

## Operational Notes

- The `run-wordpuzzle` workflow downloads the latest release artefact by default
  (the `gh` invocation can be parameterised to use a specific tag).
- If reproducible environments are required, consider using a pinned runner
  image or pre-built runtime container with required utilities to reduce
  variability.

## Example Run (manual)

1. Ensure a release tag (for example `v1.0.0`) has been created and the build
   workflow attached `wordpuzzle-release.tar.gz` to the Release.
2. From **Actions > Run workflow**, supply `SIZE=6`, `LETTERS=cadevrsoi`, and
   `REPEATS=false`.
3. The `run-wordpuzzle` job will download the artefact, extract the binary and
   `dictionary`, and execute:

```bash
./wordpuzzle --size=6 --letters=cadevrsoi --dictionary=dictionary
```

## Recommendations

1. Provide a pre-baked minimal runtime container image containing `gh`, `tar`,
   and `ca-certificates` to avoid installing packages at job runtime.
2. Optionally publish artefact checksums or signatures and verify them in the
   run workflow before executing binaries.

## Change Log

- 2026-02-25: Converted to specification style and aligned with current
  workflows `.github/workflows/cicd.yml` and
  `.github/workflows/run-wordpuzzle.yml`.
