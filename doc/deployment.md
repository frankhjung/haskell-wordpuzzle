# Run WordPuzzle on GitHub

## Objective

To be able to run the WordPuzzle application via a GitHub action with
parameters.

I want you to plan alternatives where the run artifact is a native Linux executable. You
can assume that the build pipeline produces a native Linux executable. Look at
containers, native images, etc. and propose the best alternative. Can this be
run as a GitHub action? Do I need to build a runtime container?

## Requirements

* Build pipeline `.github/workflows/haskell.yml` should be triggered by a push to any branch.
* The build pipeline `.github/workflows/haskell.yml` should prepare are release on GitHub when a release tag is pushed.
* The `run-wordpuzzle` action should be manually triggered
* The `run-wordpuzzle` action should use a release artifact from a build pipeline (i.e. it does not build the application itself).
* The `run-wordpuzzle` action should use a release artifact from a build pipeline (from a release tag).
* The `run-wordpuzzle` action should run on a minimal image.
* Can we use `cabal install` for packaging the executable and dictionary?

## Context

The application is built using Cabal and produces an executable. The
application is run using:

`./wordpuzzle --size=${{SIZE}} --letters=${{LETTERS}} --repeats`.

I want to be able to run the application as a GitHub action. The action should
be manually triggered and should use a release artifact from a build pipeline.
The build artifact should be an executable. The action should run on a minimal
image.

### Build a Release

The build pipeline should produce a release artifact only on a release tag. It will look something like:

```yaml
---
name: Build, Test, and Release

on:
  push:
    branches:
      - "**"
    tags:
      - "v*"
  pull_request:

env:
  STACK_ROOT: ${{ github.workspace }}/.stack-work

jobs:
  build-test-release:
    runs-on: ubuntu-latest
    permissions:
      contents: write

      - name: checkout
        uses: actions/checkout@v3

      ... steps build and test ...

      - name: Create/Update Release
        if: startsWith(github.ref, 'refs/tags/')
        uses: softprops/action-gh-release@v1
        with:
          files: [[path to executable]], [[path to dictionary]]
          tag_name: ${{ github.ref_name }}
          draft: false
          prerelease: false
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}

      ... steps to publish haddock to pages ...
```

### Run the Release

To run the executable, we need to download the release artifact and then run the executable. It will look something like:

```yaml
---
name: Run Wordpuzzle

on:
  workflow_dispatch:
    inputs:
      SIZE:
        description: "Minimum word size"
        required: false
        default: 4
        type: number
      LETTERS:
        description: "Letters to use"
        required: true
        type: string
      REPEATS:
        description: "Allow repeated letters"
        required: false
        default: true
        type: boolean

jobs:
  run-wordpuzzle:
    runs-on: ubuntu-latest
    permissions:
      contents: read

      # we may not need this if release artefact contains the dictonary and executable
      - name: checkout
        uses: actions/checkout@v3

      # download release artefact (need to include dictionary and executable)
      - name: Download release artefact
        run: |
          # Get the latest release download URL from GitHub API
          DOWNLOAD_URL=$(curl -s https://api.github.com/repos/${{ github.repository }}/releases/latest | \
            grep 'browser_download_url' | grep 'wordpuzzle' | cut -d '"' -f 4)
          # Check if we got a valid URL
          if [ -z "$DOWNLOAD_URL" ]; then
            echo "Error: Could not find wordpuzzle in latest release"
            exit 1
          fi
          # Download the artefact
          curl -L --fail -o wordpuzzle-artefact "$DOWNLOAD_URL"
          echo "EXE_PATH=$(pwd)/wordpuzzle-artefact" >> $GITHUB_ENV

      - name: Run main with size ${{ inputs.SIZE }} and letters ${{ inputs.LETTERS }}
        env:
          SIZE: ${{ inputs.SIZE }}
          LETTERS: ${{ inputs.LETTERS }}
          REPEATS: ${{ inputs.REPEATS }}
        run: |
          # Extract leading integer component from SIZE
          SIZE_INT="${SIZE%%[^0-9]*}"
          # Normalize letters to lowercase
          LETTERS_LOWER="${LETTERS,,}"
          # Build argument array
          ARGS=("--size=$SIZE_INT" "--letters=$LETTERS_LOWER")
          # Add repeats flag if true
          [[ "$REPEATS" = "true" ]] && ARGS+=(--repeats)
          # Run the executable with the appropriate arguments
          ${{ env.EXE_PATH }} --dictionary=./dictionary --"${ARGS[@]}"
```
