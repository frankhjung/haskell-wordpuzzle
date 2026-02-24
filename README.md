# Haskell 9 Letter Word Puzzle Solver

Solve 9 letter word puzzles like:

- [Nine Letter Word](http://nineletterword.tompaton.com/adevcrsoi/)
- [Your Word Life](http://www.yourwiselife.com.au/games/9-letter-word/)
- [NYT Spelling Bee](https://www.nytimes.com/puzzles/spelling-bee) (using
  `--repeats`)

![nineletterword.tompaton.com](doc/nineletterword.png)

Here we are using a subset of the British dictionary from the
[wbritish](https://packages.debian.org/sid/text/wbritish) package.

## Documentation

Generated haddock gets written to a build-specific path under `dist-newstyle`.
The exact subdirectory depends on the GHC version and architecture, so we use a
glob to copy the now‑famous `wordpuzzle` html output into the repository tree
for browsing.

```bash
cp -r \
  dist-newstyle/build/*/wordpuzzle-*/x/wordpuzzle/doc/html/wordpuzzle/wordpuzzle/* \
  doc/html/wordpuzzle/
```

(Alternatively run `make doc`, which performs the copy automatically.)

- [GitHub](https://frankhjung.github.io/haskell-wordpuzzle/)
  - [haddock function documentation](https://frankhjung.github.io/haskell-wordpuzzle/index.html)
  - [criterion performance measurements](https://frankhjung.github.io/haskell-wordpuzzle/benchmark.html)
- [GitLab](https://frankhjung1.gitlab.io/haskell-wordpuzzle/)
  - [haddock function documentation](https://frankhjung1.gitlab.io/haskell-wordpuzzle/index.html)
  - [criterion performance measurements](https://frankhjung1.gitlab.io/haskell-wordpuzzle/benchmark.html)

## Pipelines

- [GitHub](https://github.com/frankhjung/haskell-wordpuzzle/actions)
- [GitLab](https://gitlab.com/frankhjung1/haskell-wordpuzzle/pipelines)

### Running from GitHub Actions

A manual workflow (`.github/workflows/run-wordpuzzle.yml`) is available that
fetches the release artefact produced by the build pipeline and executes it on a
minimal runner. Dispatch the workflow from the **Actions** tab and supply the
following inputs:

- `SIZE` – minimum word size (defaults to 4)
- `LETTERS` – letters to use (first character becomes the mandatory letter)
- `REPEATS` – boolean flag to allow repeated letters (Spelling Bee)

The workflow simply mirrors the local command line usage, e.g.:

```bash
./wordpuzzle --size=6 --letters=cadevrsoi --repeats
```

See `doc/deployment.md` for more background on how the run pipeline is
constructed and how the release artefact is packaged.

## Package Dependencies

To include a package:

- update cabal sections with package with version
- run `cabal update`
- run `cabal build`
- run `make clean default`

## Build

This project can be built using either [cabal](https://www.haskell.org/cabal/)
or [stack](https://docs.haskellstack.org/en/stable/).

The default [Makefile](Makefile) builds using the
[cabal](https://www.haskell.org/cabal/) tool.

To build using [stack](https://docs.haskellstack.org/en/stable/):

```bash
make -f stack.mak [target]
```

Stack is required for GitHub pipelines as it provides the correct Haskell
version.

### Upgrading GHC

When updating the GHC version:

1. Run `make cleanall` to remove the old build artifacts.
1. Remove the `cabal.project.freeze` file.
1. Update the `cabal.project` file with the new LTS version.
1. Run `cabal update` to update the package list.
1. Run `make` to rebuild the project.
1. Run `cabal freeze` to create a new `cabal.project.freeze` file.

## Solver

This program is used to list all words from this popular puzzle. A brief outline
of what this program does is:

- get user input of:
  - minimum word length
  - letters as one string (where the **first** character is the mandatory
    letter)
  - (optional) dictionary to use to search for matching words

- print each word in dictionary that satisfies:
  - word is greater than or equal to minimum character length
  - word contains mandatory character (the first letter of the input string)
  - word contains other characters in correct frequencies
- when `--repeats` is specified there is no 9‑letter upper bound; longer words
  are allowed, as in the NYT Spelling Bee variant

## Validation

The project uses the
[validation](https://hackage.haskell.org/package/validation) library to provide
comprehensive error reporting for command line parameters. Instead of stopping
at the first error, it collects all validation failures and reports them
together.

Validations performed include:

- Minimum word size must be between 4 and 9 letters (inclusive).
- Letters string must contain between 4 and 9 unique lowercase letters (no
  repeated letters).

## How to run

For example, to call word puzzle with custom letters and dictionary:

```bash
make ARGS='-s 6 -l cadevrsoi -d /usr/share/dict/words' exec
```

### 9-Letter example

For example, to call word puzzle (no letter repeats, minimum word size 6) with
custom letters:

```bash
make ARGS='-s 6 -l cadevrsoi' exec
```

Or run using default dictionary:

```bash
make ARGS='-s 6 -l cadevrsoi' exec
```

### Spelling Bee (repeats) example

When `--repeats` is enabled the solver allows letters to repeat and there is no
hard 9‑letter upper bound — longer words from the dictionary may be returned.
Example (run with repeats enabled):

In this example, the solver will print 7 letter words using letters "mitncao"
with repeats allowed:

```bash
make ARGS='-s 7 -l mitncao -r' exec
```

### Help

Get command line help:

```bash
$ wordpuzzle --help
https://github.com/frankhjung/haskell-wordpuzzle

Usage: wordpuzzle [-s|--size INT] (-l|--letters STRING)
                  [-d|--dictionary FILENAME] [-r|--repeats]

  Solve word puzzles

Available options:
  -s,--size INT            Minimum word size (value from 4..9) (default: 4)
  -l,--letters STRING      Letters to make words (4 to 9 unique lowercase
                           letters)
  -d,--dictionary FILENAME Dictionary to search for words
                           (default: "dictionary")
  -r,--repeats             Allow letters to repeat (like Spelling Bee)
  -h,--help                Show this help text

Version: 1.0.0
```

Or call without command line arguments:

```bash
$ cabal exec wordpuzzle --
Missing: (-l|--letters STRING)

Usage: wordpuzzle [-s|--size INT] (-l|--letters STRING)
                  [-d|--dictionary FILENAME] [-r|--repeats]

  Solve word puzzles
```

### Default Dictionary

When specifying a dictionary use (default is "dictionary"):

```bash
cabal exec wordpuzzle -- -l cadevrsoi -ddictionary
```

### Sort Words by Size

To show words by size use:

```bash
cabal exec wordpuzzle -- -l cadevrsoi | gawk '{print length($0), $0;}' | sort -r
```

## Unit Tests

Using [HSpec](https://hspec.github.io/):

```text
$ cabal test --test-show-details=direct
...
Finished in 0.00xx seconds
23 examples, 0 failures
Test suite test: PASS
```

## Performance

### Benchmarks

To enable benchmarks with [Cabal](https://www.haskell.org/cabal/) call:

```bash
cabal configure --enable-benchmarks
```

Then to run Criterion benchmarks call:

```bash
cabal bench
```

```text
$ cabal bench
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - wordpuzzle-1.0.0 (bench:benchmark) (ephemeral targets)
Preprocessing benchmark 'benchmark' for wordpuzzle-1.0.0...
Building benchmark 'benchmark' for wordpuzzle-1.0.0...
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking WordPuzzle/nineLetters
time                 224.3 ns   (221.5 ns .. 226.6 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 224.9 ns   (222.5 ns .. 226.4 ns)
std dev              6.349 ns   (4.298 ns .. 9.762 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking WordPuzzle/spellingBee
time                 171.1 ns   (170.9 ns .. 171.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 170.9 ns   (170.8 ns .. 171.1 ns)
std dev              493.2 ps   (404.7 ps .. 634.4 ps)

Benchmark benchmark: FINISH
```

### Execution Summary

Using the dictionary sited above, the run time performance for the example:

```text
$ cabal exec wordpuzzle -- -s 7 -l cadevrsoi -ddictionary +RTS -s 1>/dev/null
     858,788,568 bytes allocated in the heap
       5,242,528 bytes copied during GC
         118,560 bytes maximum residency (2 sample(s))
          33,600 bytes maximum slop
               6 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       206 colls,     0 par    0.005s   0.005s     0.0000s    0.0001s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.241s  (  0.241s elapsed)
  GC      time    0.005s  (  0.005s elapsed)
  EXIT    time    0.000s  (  0.003s elapsed)
  Total   time    0.247s  (  0.250s elapsed)

  Alloc rate    3,557,834,378 bytes per MUT second

  Productivity  97.7% of total user, 96.4% of total elapsed
```

## Command Line Parsers

Apart from solving a simple word puzzle this project also explores the following
command line parsers:

- [System.Environment getArgs](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [System.Console GetOpt](https://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

Each is preserved in a separate Git
[branch](https://github.com/frankhjung/haskell-wordpuzzle/branches).

## Package Version

The version is dynamically included from the
[Cabal](https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code)
configuration file.

Version 1.0.0 of this project is using [LTS Haskell 22.44
(ghc-9.6.6)](https://www.stackage.org/lts-22.44)

## Dependencies Graph

To produce a package dependencies graph, run:

```bash
stack dot --external | dot -Tpng -o doc/dependencies.png
```

![Dependencies Graph](doc/dependencies.png)

## References

### Haskell Build

Either of the following build tools can be used:

- [Cabal](https://www.haskell.org/cabal/)
- [Stack](https://docs.haskellstack.org/en/stable/)

### Haddock Documentation

Documentation is built using [Haddock](https://www.haskell.org/haddock/).

Included in this repository is generated documentation for the current version.

- [haddock function documentation](./doc/html/wordpuzzle/index.html)
- [criterion performance benchmarks](./doc/benchmark.html)

## Other Implementations

- [Clojure](https://gitlab.com/frankhjung1/clojure-wordpuzzle)
- [Haskell](https://gitlab.com/frankhjung1/haskell-wordpuzzle)
- [Java](https://gitlab.com/frankhjung1/java-wordpuzzle)
- [Kotlin](https://gitlab.com/frankhjung1/kotlin-wordpuzzle)
- [Go](https://gitlab.com/frankhjung1/go-wordpuzzle)
- [Python](https://gitlab.com/frankhjung1/python-wordpuzzle)
