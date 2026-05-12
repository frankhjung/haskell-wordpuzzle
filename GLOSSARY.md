# Glossary

## Puzzle

The core domain concept representing the constraints of a word search. A Puzzle
consists of a **Mandatory Letter**, a **Letter Pool**, and a **Word Size**.

## WordPuzzle Type

The central data structure used in the Haskell implementation to store the
parsed command-line arguments. It includes the minimum word size, the mandatory
character, the valid letter pool, the file path to the dictionary, and a flag
indicating if repeated letters are allowed.

## Mandatory Letter

The specific character that MUST be present in every valid word found.

## Letter Pool

The set of allowed characters for the puzzle (usually between 4 and 9 unique
lowercase letters).

## Word Size

The minimum length constraint for a valid word.

## Dictionary

A collection of candidate words (represented as `ByteString`s) to be evaluated
against a **Puzzle**.

## Dictionary Filtering

The process of normalising and filtering a raw dictionary file into candidate
words. A utility script enforces these criteria: words must only contain
lowercase alphabetic characters and have a minimum length of four. Roman
numerals, hyphenated words, words with uppercase letters, and words containing
numbers are all excluded.

## Solver

The core matching mechanism, implemented as a stream transformer, that evaluates
a **Dictionary** (as an `InputStream ByteString`) against a **Puzzle** to
produce a matching stream of words. It is decoupled from physical I/O details
like file paths or console output.

## Repeats

A puzzle mode (like the NYT Spelling Bee) where letters in the **Letter Pool**
may be used multiple times in a single word.

## Nine Letters

A puzzle mode where words are limited to a maximum length of nine characters,
and each letter in the **Letter Pool** may be used at most once. This is the
default behaviour when **Repeats** are not allowed.

## Validation Error

An error produced when invalid parameters are provided to the puzzle, such as an
invalid **Word Size** or **Letter Pool**.

## Command-Line Parser

The application uses the `optparse-applicative` library to parse command-line
arguments. It provides an applicative interface for defining the parameters
(`size`, `letters`, `dictionary`, `repeats`) and generating help text.

## CI/CD Workflows

The project defines continuous integration and continuous deployment pipelines
using the following key jobs:

- **build_and_test**: Automatically builds, tests, and benchmarks the codebase.
- **publish_pages**: Deploys Haddock documentation and benchmarks to pages.
- **package_and_release**: Packages the binary and dictionary into a release.
- **run_wordpuzzle**: A manual workflow to run the solver interactively.
