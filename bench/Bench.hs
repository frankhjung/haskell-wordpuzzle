module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (nineLetters, spellingBee)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "nineLetters" $ nf (nineLetters "cadevrsoi") "aardvarks"
    , bench "spellingBee" $ nf (spellingBee "mitncao") "atomic"
    ]
  ]
