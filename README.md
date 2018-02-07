# Haskell 9 Letter Word Puzzle Solver

Solve 9 letter word puzzles like:

* [Nine Letter Word](http://nineletterword.tompaton.com/adevcrsoi/)
* [Your Word Life](http://www.yourwiselife.com.au/games/9-letter-word/)

Here we are using a subset of the British dictionary from the
[wbritish](https://packages.debian.org/sid/text/wbritish) package.

## Method

A brief outline of this program is:

* get user input of:
  * letters as one string (first letter is the mandatory letter)
  * a dictionary
  * [optional] minimum word length (default is 4)

* print each word in dictionary that satisfies:
  * word is greater than or equal to minimum character length
  * word contain mandatory character
  * word contains other characters in correct frequencies

## How to run

```bash
# specifying a dictionary
stack exec -- wordpuzzle 4 c adevcrsoi dictionary
```

To show in word size order use:

```bash
stack exec -- wordpuzzle 4 c adevcrsoi dictionary | gawk '{if (length($0) < 10) print length($0), $0; | "sort" }'
```

