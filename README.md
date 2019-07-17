# Haskell 9 Letter Word Puzzle Solver

Solve 9 letter word puzzles like:

* [Nine Letter Word](http://nineletterword.tompaton.com/adevcrsoi/)
* [Your Word Life](http://www.yourwiselife.com.au/games/9-letter-word/)

![nineletterword.tompaton.com](doc/nineletterword.png)

Here we are using a subset of the British dictionary from the
[wbritish](https://packages.debian.org/sid/text/wbritish) package.


## Documentation

See GitLab pages documentation
[here](https://frankhjung1.gitlab.io/haskell-wordpuzzle/) which includes:

* [haddock function documentation](https://frankhjung1.gitlab.io/haskell-wordpuzzle/html/wordpuzzle/index.html)
* [criterion performance measurements](https://frankhjung1.gitlab.io/haskell-wordpuzzle/benchmark.html)
* [test coverage](https://frankhjung1.gitlab.io/haskell-wordpuzzle/hpc/wordpuzzle/test/hpc_index.html)


## Method

This program is used to list all words from this popular puzzle.
A brief outline of what this program does is:

* get user input of:
  * minimum word length
  * mandatory letter required in each word
  * letters as one string
  * [optional] dictionary to use to search for matching words

* print each word in dictionary that satisfies:
  * word is greater than or equal to minimum character length
  * word contain mandatory character
  * word contains other characters in correct frequencies


## How to run

### Help

Get command line help:

```bash
$ wordpuzzle --help
https://github.com/frankhjung/haskell-wordpuzzle

Usage: wordpuzzle [-s|--size INT] (-m|--mandatory CHAR) (-l|--letters STRING)
                  [-d|--dictionary FILENAME]
  Solve word puzzles like those at nineletterword.tompaton.com

Available options:
  -s,--size INT            Minimum word size (default: 4)
  -m,--mandatory CHAR      Mandatory character for all words
  -l,--letters STRING      String of letters to make words
  -d,--dictionary FILENAME Alternate dictionary (default: "dictionary")
  -h,--help                Show this help text

Version: 0.6.1
```

Or call without command line arguments:

```bash
$ wordpuzzle
Missing: (-m|--mandatory CHAR) (-l|--letters STRING)

Usage: wordpuzzle [-s|--size INT] (-m|--mandatory CHAR) (-l|--letters STRING)
                  [-d|--dictionary FILENAME]
  Solve word puzzles like those at nineletterword.tompaton.com
```

### Default Dictionary

When specifying a dictionary use (default is "dictionary"):

```bash
wordpuzzle -s 4 -m c -l adevcrsoi -ddictionary
```

### Sort Words by Size

To show words by size use:

```bash
wordpuzzle -s 4 -m c -l adevcrsoi | gawk '{print length($0), $0;}' | sort -r
```


## Unit Tests

Using [HSpec](https://hspec.github.io/):

```text
wordpuzzle-0.6.1: test (suite: test)

Progress 1/2: wordpuzzle-0.6.1
remove
  when character is in list
    returns list less that character
  when character is in list twice
    returns list less one instance of that character
  when character is not in list
    returns original list
isValid
  when word containing characters
    returns true
  when word containing a valid subset of characters
    returns true
  when word does not contain valid characters
    returns false
  when word does not contain valid character frequency
    returns false

Finished in 0.0010 seconds
7 examples, 0 failures

wordpuzzle-0.6.1: Test suite test passed
Generating coverage report for wordpuzzle's test-suite "test"
100% expressions used (30/30)
100% boolean coverage (2/2)
     100% guards (0/0)
     100% 'if' conditions (2/2)
     100% qualifiers (0/0)
100% alternatives used (9/9)
100% local declarations used (2/2)
100% top-level declarations used (2/2)
```


## Performance

### Benchmarks

Running Criterion benchmarks:

```text
wordpuzzle-0.6.1: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking WordPuzzle/isValid
time                 10.36 ns   (10.31 ns .. 10.42 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 10.38 ns   (10.32 ns .. 10.51 ns)
std dev              278.6 ps   (146.5 ps .. 477.4 ps)
variance introduced by outliers: 45% (moderately inflated)

benchmarking WordPuzzle/remove
time                 10.79 ns   (10.75 ns .. 10.83 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.78 ns   (10.75 ns .. 10.82 ns)
std dev              120.7 ps   (87.04 ps .. 178.6 ps)
variance introduced by outliers: 12% (moderately inflated)

Benchmark benchmark: FINISH
```

### Execution Summary

Using the dictionary sited above, the run time performance for the example:

```text
$ wordpuzzle -s 4 -m c -l adevcrsoi -ddictionary +RTS -s 1>/dev/null
     309,244,536 bytes allocated in the heap
         247,576 bytes copied during GC
         123,928 bytes maximum residency (6 sample(s))
          28,072 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       288 colls,     0 par    0.002s   0.002s     0.0000s    0.0001s
  Gen  1         6 colls,     0 par    0.000s   0.000s     0.0000s    0.0001s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.110s  (  0.109s elapsed)
  GC      time    0.003s  (  0.002s elapsed)
  EXIT    time    0.001s  (  0.008s elapsed)
  Total   time    0.114s  (  0.121s elapsed)

  Alloc rate    2,820,925,034 bytes per MUT second

  Productivity  96.6% of total user, 97.0% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
```


## Command Line Parsers

Apart from solving a simple word puzzle this project also explores the following
command line parsers:

* [System.Environment getArgs](https://hackage.haskell.org/package/base/docs/System-Environment.html)
* [System.Console GetOpt](https://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html)
* [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

Each is preserved in a separate Git [branch](https://github.com/frankhjung/haskell-wordpuzzle/branches).


## Package Version

The version is dynamically included from the
[Cabal](https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code)
configuration file.

Project is using [LTS Haskell 12.26 (ghc-8.4.4)](https://www.stackage.org/lts-12.26)


## Dependencies Graph

To produce a package dependencies graph, run:

```bash
stack dot --external | dot -Tpng -o doc/dependencies.png
```

![Dependencies Graph](doc/dependencies.png)


## References

Documentation is built using [Haddock](https://www.haskell.org/haddock/).

Included in this repository is generated documentation for the current version.

* [haddock function documentation](./doc/html/wordpuzzle/index.html)
* [criterion performance benchmarks](./doc/benchmark.html)
* [test coverage](./doc/hpc/wordpuzzle/test/hpc_index.html)

