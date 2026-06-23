# Change Log

All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Add change log.

### Changed

- Update `stack.mk` help, dependency graph, and dictionary.
- Update Cabal `Makefile` so both are consistent.
- Update `README.md` to include Lean 4.
- Update `docs/design.md`.

## [v1.1.1] - 2026-06-23

### Added

- Mandatory letter feature and update related documentation.

Previously, the mandatory character was hardcoded as the first character of the
`letters` argument. This has been updated to:

- Require a separate `-m` argument.
- Verify that the mandatory letter is present in the `letters` pool.
- Validate that both are lowercase alphabetical characters.

## [v1.1.0] - 2026-06-23

### Added

- Lean implementation documentation.

### Changed

- Update `Makefile` help and general docs.

### Refactored

- Implement smart constructor pattern for WordPuzzle.

## [v1.0.3] - 2026-05-12

### Refactored

- Improve solve and nineLetters function implementations.

## [v1.0.2] - 2026-03-09

### Changed

- Bump version to 1.0.2 and update documentation.
- Remove Roman numerals from dictionary.
- Improve pipelines.
- Add examples with and without letter repeats.

## [v1.0.1] - 2026-03-08

### Changed

- Dynamic make help text.
- Update CI/CD graph.

## [v1.0.0] - 2026-02-24

### Added

- First stable release.
