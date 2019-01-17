# Haskell 9 Letter Word Puzzle Solver

Solve 9 letter word puzzles like:

* [Nine Letter Word](http://nineletterword.tompaton.com/adevcrsoi/)
* [Your Word Life](http://www.yourwiselife.com.au/games/9-letter-word/)

![nineletterword.tompaton.com](doc/nineletterword.png)

Here we are using a subset of the British dictionary from the
[wbritish](https://packages.debian.org/sid/text/wbritish) package.


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
  * [optional] reject plurals


## How to run

### Help

Get command line help:

```bash
$ wordpuzzle --help
https://github.com/frankhjung/haskell-wordpuzzle

Usage: wordpuzzle [-s|--size INT] (-m|--mandatory CHAR) (-l|--letters STRING)
                  [-d|--dictionary FILENAME] [-p|--plurals]
  Solve word puzzles like those at nineletterword.tompaton.com

Available options:
  -s,--size INT            Minimum word size (default: 4)
  -m,--mandatory CHAR      Mandatory character for all words
  -l,--letters STRING      String of letters to make words
  -d,--dictionary FILENAME Alternate dictionary (default: "dictionary")
  -p,--plurals             Include plural words
  -h,--help                Show this help text

Version: 0.5.2
```

Or call without command line arguments:

```bash
wordpuzzle
Missing: (-m|--mandatory CHAR) (-l|--letters STRING)

Usage: wordpuzzle [-s|--size INT] (-m|--mandatory CHAR) (-l|--letters STRING)
                  [-d|--dictionary FILENAME] [-p|--plurals]
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
wordpuzzle-0.5.2: test (suite: test)

Progress 1/2: wordpuzzle-0.5.2
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
isPlural
  when word ends in 'ss'
    returns false
  when word does not end in 's'
    returns false
  when word ends in 's'
    returns true

Finished in 0.0011 seconds
10 examples, 0 failures

wordpuzzle-0.5.2: Test suite test passed
```


## Performance

### Benchmarks

Running Criterion benchmarks:

```text
wordpuzzle-0.5.2: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking WordPuzzle/isPlural
time                 40.48 ns   (40.32 ns .. 40.75 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 40.39 ns   (40.26 ns .. 40.71 ns)
std dev              589.9 ps   (302.3 ps .. 1.147 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking WordPuzzle/isValid
time                 10.83 ns   (10.79 ns .. 10.88 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 10.81 ns   (10.78 ns .. 10.87 ns)
std dev              144.7 ps   (113.4 ps .. 204.0 ps)
variance introduced by outliers: 16% (moderately inflated)

benchmarking WordPuzzle/remove
time                 11.59 ns   (11.06 ns .. 12.36 ns)
                     0.985 R²   (0.975 R² .. 0.999 R²)
mean                 11.29 ns   (11.03 ns .. 11.85 ns)
std dev              1.170 ns   (640.2 ps .. 1.854 ns)
variance introduced by outliers: 93% (severely inflated)

Benchmark benchmark: FINISH
Completed 2 action(s).
```

### Execution Summary

Using the dictionary sited above, the run time performance for the example:

```text
$ wordpuzzle -s 4 -m c -l adevcrsoi -ddictionary +RTS -s 1>/dev/null
     260,426,480 bytes allocated in the heap
         232,128 bytes copied during GC
         120,128 bytes maximum residency (5 sample(s))
          28,088 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       241 colls,     0 par    0.002s   0.002s     0.0000s    0.0003s
  Gen  1         5 colls,     0 par    0.000s   0.000s     0.0000s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.172s  (  0.171s elapsed)
  GC      time    0.002s  (  0.002s elapsed)
  EXIT    time    0.001s  (  0.006s elapsed)
  Total   time    0.176s  (  0.181s elapsed)

  Alloc rate    1,517,316,875 bytes per MUT second

  Productivity  98.0% of total user, 98.2% of total elapsed

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


## Documentation

Documentation is built using [Haddock](https://www.haskell.org/haddock/).

Included in this repository is generated documentation for the current version.

* [haddock function documentation](./doc/html/wordpuzzle/index.html)
* [criterion performance benchmarks](./doc/benchmark.html)
* [test coverage](./doc/hpc/wordpuzzle/test/hpc_index.html)

