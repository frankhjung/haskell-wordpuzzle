module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (hasLetters)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "hasLetters" $ nf hasLetters "foobar"
    ]
  ]
