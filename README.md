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

Version: 0.5.1
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
wordpuzzle-0.5.1: test (suite: test)

Progress 1/2: wordpuzzle-0.5.1
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

Finished in 0.0008 seconds
10 examples, 0 failures

wordpuzzle-0.5.1: Test suite test passed
```


## Performance

### Benchmarks

Running Criterion benchmarks:

```text
wordpuzzle-0.5.1: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking WordPuzzle/isPlural
time                 40.51 ns   (40.36 ns .. 40.67 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 40.46 ns   (40.31 ns .. 40.82 ns)
std dev              697.6 ps   (423.4 ps .. 1.386 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking WordPuzzle/isValid
time                 10.84 ns   (10.80 ns .. 10.90 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 10.84 ns   (10.78 ns .. 11.02 ns)
std dev              314.4 ps   (113.6 ps .. 630.1 ps)
variance introduced by outliers: 48% (moderately inflated)

benchmarking WordPuzzle/remove
time                 10.88 ns   (10.85 ns .. 10.91 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.85 ns   (10.82 ns .. 10.90 ns)
std dev              129.4 ps   (90.80 ps .. 183.9 ps)
variance introduced by outliers: 13% (moderately inflated)

Benchmark benchmark: FINISH
Completed 22 action(s).
```

### Execution Summary

Using the dictionary sited above, the run time performance for the example:

```text
$ wordpuzzle -s 4 -m c -l adevcrsoi -ddictionary +RTS -s 1>/dev/null

     260,425,528 bytes allocated in the heap
         232,680 bytes copied during GC
         120,096 bytes maximum residency (5 sample(s))
          28,304 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       241 colls,     0 par    0.002s   0.002s     0.0000s    0.0001s
  Gen  1         5 colls,     0 par    0.000s   0.000s     0.0000s    0.0001s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.166s  (  0.166s elapsed)
  GC      time    0.002s  (  0.002s elapsed)
  EXIT    time    0.001s  (  0.002s elapsed)
  Total   time    0.169s  (  0.170s elapsed)

  Alloc rate    1,570,307,266 bytes per MUT second

  Productivity  98.2% of total user, 98.3% of total elapsed

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

