# Glossary

## Puzzle

The core domain concept representing the constraints of a word search. A Puzzle
consists of a **Mandatory Letter**, a **Letter Pool**, and a **Word Size**.

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

## Solver

The core matching mechanism, implemented as a stream transformer, that evaluates
a **Dictionary** (as an `InputStream ByteString`) against a **Puzzle** to
produce a matching stream of words. It is decoupled from physical I/O details
like file paths or console output.

## Repeats

A puzzle mode (like the NYT Spelling Bee) where letters in the **Letter Pool**
may be used multiple times in a single word.
