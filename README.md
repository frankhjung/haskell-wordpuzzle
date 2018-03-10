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


## How to run

Get command line help:

```bash
stack exec -- wordpuzzle --help

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

Version: 0.3.0
```

When specifying a dictionary use (default is "dictionary"):

```bash
stack exec -- wordpuzzle -s 4 -m c -l adevcrsoi -ddictionary
```

To show words by size use:

```bash
stack exec -- wordpuzzle -s 4 -m c -l adevcrsoi | gawk '{print length($0), $0;}' | sort -r
```


## Command Line Parsers

Apart from solving a simple word puzzle this project also explores the following
command line parsers:

* [System.Environment getArgs](https://hackage.haskell.org/package/base/docs/System-Environment.html)
* [System.Console GetOpt](https://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html)
* [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

Each is preserved in a separate Git [branch](https://github.com/frankhjung/haskell-wordpuzzle/branches).


## Documentation

Documentation is built using [Haddock](https://www.haskell.org/haddock/):

![Example: wordpuzzle library](doc/wordpuzzle.png)
