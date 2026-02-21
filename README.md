# Haskell 9 Letter Word Puzzle Solver

Solve 9 letter word puzzles like:

- [Nine Letter Word](http://nineletterword.tompaton.com/adevcrsoi/)
- [Your Word Life](http://www.yourwiselife.com.au/games/9-letter-word/)

![nineletterword.tompaton.com](doc/nineletterword.png)

Here we are using a subset of the British dictionary from the
[wbritish](https://packages.debian.org/sid/text/wbritish) package.

## Documentation

- [GitHub](https://frankhjung.github.io/haskell-wordpuzzle/)
  - [haddock function documentation](https://frankhjung.github.io/haskell-wordpuzzle/index.html)
  - [criterion performance measurements](https://frankhjung.github.io/haskell-wordpuzzle/benchmark.html)
- [GitLab](https://frankhjung1.gitlab.io/haskell-wordpuzzle/)
  - [haddock function documentation](https://frankhjung1.gitlab.io/haskell-wordpuzzle/index.html)
  - [criterion performance measurements](https://frankhjung1.gitlab.io/haskell-wordpuzzle/benchmark.html)

## Pipelines

- [GitHub](https://github.com/frankhjung/haskell-wordpuzzle/actions)
- [GitLab](https://gitlab.com/frankhjung1/haskell-wordpuzzle/pipelines)

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

## Validation

The project uses the
[validation](https://hackage.haskell.org/package/validation) library to provide
comprehensive error reporting for command line parameters. Instead of stopping
at the first error, it collects all validation failures and reports them
together.

Validations performed include:

- Minimum word size must be in range 1..9.
- Letters string must contain 4 to 9 unique lowercase characters.
- Letters string must not be empty.

## How to run

### Using Make

For example, to call word puzzle with custom letters and dictionary:

```bash
make ARGS='-s 6 -l cadevrsoi -d /usr/share/dict/words' exec
```

Or run using default dictionary:

```bash
make ARGS='-s 6 -l cadevrsoi' exec
```

### Help

Get command line help:

```bash
$ wordpuzzle --help
https://github.com/frankhjung/haskell-wordpuzzle

Usage: wordpuzzle [-s|--size INT] (-l|--letters STRING)
                  [-d|--dictionary FILENAME]

  Solve word puzzles

Available options:
  -s,--size INT            Minimum word size (value from 1..9) (default: 4)
  -l,--letters STRING      Letters to make words (4 to 9 unique lowercase
                           letters)
  -d,--dictionary FILENAME Dictionary to search for words
                           (default: "dictionary")
  -h,--help                Show this help text

Version: 1.0.0
```

Or call without command line arguments:

```bash
$ wordpuzzle
Missing: (-l|--letters STRING)

Usage: wordpuzzle [-s|--size INT] (-l|--letters STRING)
                  [-d|--dictionary FILENAME]
  Solve word puzzles
```

### Default Dictionary

When specifying a dictionary use (default is "dictionary"):

```bash
wordpuzzle -l cadevrsoi -ddictionary
```

### Sort Words by Size

To show words by size use:

```bash
wordpuzzle -l cadevrsoi | gawk '{print length($0), $0;}' | sort -r
```

## Unit Tests

Using [HSpec](https://hspec.github.io/):

```text
checkSize
  size outside range
    returns Left [✔]
  size outside range
    returns Left [✔]
  size in range
    returns Right [✔]
checkLetters
  fewer than 4 letters
    returns Left [✔]
  4 lowercase letters (lower bound)
    returns Right [✔]
  mid-range lowercase letters
    returns Right [✔]
  9 lowercase letters (upper bound)
    returns Right [✔]
  mixed case letters
    returns Left [✔]
  duplicate characters
    returns Left [✔]
  too many letters
    returns Left [✔]
hasLetters
  when word contains valid characters
    returns true [✔]
  when word contains a valid subset of characters
    returns true [✔]
  when word does not contain valid characters
    returns false [✔]
  when word does not contain valid character frequency
    returns false [✔]
hasLetters'
  when word contains valid characters
    returns true [✔]
  when word contains a valid subset of characters
    returns true [✔]
  when word does not contain valid characters
    returns false [✔]
  when word does not contain valid character frequency
    returns false [✔]

Finished in 0.0011 seconds
18 examples, 0 failures
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
wordpuzzle-1.0.0: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking WordPuzzle/hasLetters
time                 26.78 ns   (25.40 ns .. 27.53 ns)
                     0.978 R²   (0.965 R² .. 0.987 R²)
mean                 23.21 ns   (21.69 ns .. 25.00 ns)
std dev              5.142 ns   (4.587 ns .. 5.840 ns)
variance introduced by outliers: 98% (severely inflated)

benchmarking WordPuzzle/hasLetters'
time                 20.32 ns   (19.76 ns .. 21.00 ns)
                     0.989 R²   (0.981 R² .. 0.996 R²)
mean                 20.05 ns   (19.37 ns .. 21.02 ns)
std dev              2.662 ns   (2.067 ns .. 3.625 ns)
variance introduced by outliers: 95% (severely inflated)

Benchmark benchmark: FINISH
```

### Execution Summary

Using the dictionary sited above, the run time performance for the example:

```text
$ wordpuzzle -s 4 -l cadevrsoi -ddictionary +RTS -s 1>/dev/null
     250,717,968 bytes allocated in the heap
         202,096 bytes copied during GC
         119,944 bytes maximum residency (5 sample(s))
          27,816 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       231 colls,     0 par    0.004s   0.004s     0.0000s    0.0007s
  Gen  1         5 colls,     0 par    0.001s   0.001s     0.0003s    0.0004s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.007s elapsed)
  MUT     time    0.122s  (  0.134s elapsed)
  GC      time    0.005s  (  0.005s elapsed)
  EXIT    time    0.001s  (  0.004s elapsed)
  Total   time    0.129s  (  0.150s elapsed)

  Alloc rate    2,053,382,211 bytes per MUT second

  Productivity  95.5% of total user, 91.8% of total elapsed
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
