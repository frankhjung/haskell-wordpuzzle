# Run WordPuzzle on GitLab

## Objective

To be able to run the WordPuzzle application via a GitLab pipeline with
parameters.

I want you to plan alternatives where the run artifact is a native Linux
executable. You can assume that the build pipeline produces a native Linux
executable. Look at containers, native images, etc. and propose the best
alternative. Can this be run as a GitLab pipeline? Do I need to build a runtime
container?

* Can we run multiple pipelines on GitLab?
* Can they be paramerized?
* Can they be triggered manually?

## Requirements

* Build pipeline `.gitlab-ci.yml` should be triggered by a push to any branch.
* The build pipeline `.gitlab-ci.yml` should prepare are release on GitHub when
  a release tag is pushed.
* The `run-wordpuzzle` pipeline should be manually triggered
* The `run-wordpuzzle` pipeline should use a release artifact from a build
  pipeline (i.e. it does not build the application itself).
* The `run-wordpuzzle` pipeline should use a release artifact from a build
  pipeline (from a release tag).
* The `run-wordpuzzle` pipeline should run on a minimal image.
* Can we use `cabal install` for packaging the executable and dictionary?

## Context

The application is built using Cabal and produces an executable. The application
is run using:

`./wordpuzzle --size=${{SIZE}} --letters=${{LETTERS}} --repeats`.

I want to be able to run the application as a GitLab pipeline. The pipeline
should be manually triggered and should use a release artifact from a build
pipeline. The build artifact should be an executable. The pipeline should run on
a minimal image.

### Build a Release

The GitLab build pipeline should produce a release artifact only on a release
tag.

### Run the Release

To run the executable, we need to download the release artifact and then run the
executable.

## Updates

### Review Decisions

* The pipeline will produce and package a native Linux executable.
* The `dictionary` is already included in the Git repository, so the pipeline does not need to rebuild it; it will simply package the existing file along with the executable.

### Implementation Plan (GitLab)

1. **Build Job Modifications**: The `build` job in `.gitlab-ci.yml` is configured to run on tag pushes (e.g., `v*`). It compiles the application and captures the generated `wordpuzzle` executable and the `dictionary` file as artifacts.
2. **Release Generation via GitLab Packages**: A `package_and_release` job bundles the executable and dictionary into `wordpuzzle-release.tar.gz`. It pushes this tarball to the **GitLab Generic Package Registry** and uses the `release` keyword to create a GitLab Release pointing to the package.
3. **Manual Trigger Pipeline**: The `run-wordpuzzle` job can be manually triggered directly from the GitLab web UI. It accepts variables (`SIZE`, `LETTERS`, `REPEATS`, and a `TARGET_TAG`). It downloads the specified tarball from the Package Registry, extracts the executable, and executes it with the provided parameters.
