# Run WordPuzzle on GitLab — Specification

## Purpose

Specify the behaviour and implementation details for running the pre-built
WordPuzzle native executable from a GitLab pipeline. The document describes the
current pipeline implementation, required inputs, artefact handling, and the
manual run pipeline used to execute the binary on a minimal runner image.

## Scope

This specification applies to the GitLab CI configuration in this repository
(`.gitlab-ci.yml`) and the included `run-wordpuzzle` template (see
`templates/run-wordpuzzle/template.yml`). It assumes the build pipeline produces
a native Linux executable and that the repository contains a `dictionary` file
which should be packaged with the executable.

## Definitions

- Artifact / artefact: packaged output (tarball) containing the executable and
  dictionary. Spelt as "artefact" in documentation to match project style.
- TARGET_TAG: release tag (for example `v1.0.0`) that identifies a packaged
  artefact in the GitLab Generic Package Registry.

## Requirements

1. The build pipeline shall be triggered for pushes on any branch and for tag
   pushes.
2. On tag pushes that match `^v`, the build pipeline shall produce and publish a
   release artefact containing `wordpuzzle` and `dictionary`.
3. The `run-wordpuzzle` pipeline/job shall be manually triggerable via the
   GitLab web UI (`CI_PIPELINE_SOURCE == "web"`).
4. The `run-wordpuzzle` job shall not recompile the project; it must download
   the release artefact identified by `TARGET_TAG` and execute the contained
   binary.
5. The `run-wordpuzzle` job shall run on a minimal container image and install
   only the packages required to retrieve and run the artefact (e.g. `curl`,
   `ca-certificates`, `tar`).
6. The `run-wordpuzzle` job shall accept the variables `SIZE`, `LETTERS`,
   `REPEATS` and `TARGET_TAG` as inputs and pass them to the executable.

## Current Implementation (mapping to repository files)

- `.gitlab-ci.yml`
  - Declares pipeline `spec` inputs (`target_tag`, `size`, `letters`, `repeats`)
    and maps them to CI variables (`TARGET_TAG`, `SIZE`, `LETTERS`, `REPEATS`).
  - Uses `workflow.rules` to allow manual `web` pipelines and to skip pipelines
    created for branch pushes when an MR is opened.
  - Defines `build_and_test` job which runs for non-web pipelines and produces
    `release/wordpuzzle` and `release/dictionary` as artifacts.
  - Defines `package_and_release` job which runs on tag pushes matching `^v`.
    This job packages `release/wordpuzzle` and `release/dictionary` into
    `wordpuzzle-release.tar.gz` and uploads it to the GitLab Generic Package
    Registry. It also creates a GitLab Release using the `release` keyword.
  - `publish_pages` publishes Haddock documentation for `master` pushes.

- `templates/run-wordpuzzle/template.yml`
  - Implements the `run_wordpuzzle` job that:
    - Runs on `CI_PIPELINE_SOURCE == "web"` and is suitable for manual runs.
    - Uses a minimal `debian:stable-slim` image and installs `curl` and
      `ca-certificates` at runtime.
    - Validates `TARGET_TAG` is provided, downloads the artefact from
      `${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/wordpuzzle/${TARGET_TAG}/wordpuzzle-release.tar.gz`
      using `JOB-TOKEN`, extracts it and runs `./wordpuzzle` with normalized
      `SIZE` and `LETTERS` inputs.

## Security Considerations

- `JOB-TOKEN` is used to fetch packages from the GitLab Generic Package
  Registry; this is a scoped credential available to CI jobs and is appropriate
  for this use case. Ensure project/package visibility and token scopes are
  configured correctly.
- The `run_wordpuzzle` job installs packages at runtime; to reduce attack
  surface consider building a minimal runtime container image that already
  contains required utilities, and reference it in the template.

## Operational Notes

- The `package_and_release` job runs only on tag pushes; therefore `TARGET_TAG`
  supplied to `run_wordpuzzle` must correspond to an existing tag with an
  uploaded artefact.
- `run_wordpuzzle` declares a `needs` dependency on `package_and_release` with
  `optional: true`. This allows the run job to be started independently of the
  package job when the artefact is already available in the registry.

## Example Run (manual)

1. Ensure a release tag (for example `v1.0.0`) has been created and the build
   pipeline published `wordpuzzle-release.tar.gz` to the Generic Package
   Registry.
2. From **CI / Run pipeline**, start a pipeline with `TARGET_TAG=v1.0.0`,
   `SIZE=6`, `LETTERS=cadevrsoi`, `REPEATS=false`.
3. The `run_wordpuzzle` job will download the artefact, extract the binary and
   `dictionary`, and execute:

```bash
./wordpuzzle --size=6 --letters=cadevrsoi --dictionary=dictionary
```

## Recommendations

1. Optionally provide a small runtime container image (pre-baked with `curl`,
   `tar`, `ca-certificates`) to avoid `apt-get` in CI jobs and reduce runtime
   variability.
2. Consider signing artefacts or publishing to a registry with immutability to
   protect against accidental replacement of release packages.

## Change Log

- 2026-02-25: Reworked document into a requirements/specification and aligned
  content with `.gitlab-ci.yml` and `templates/run-wordpuzzle/template.yml`.
