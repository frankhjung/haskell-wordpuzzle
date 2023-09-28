module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (hasLetters, hasLetters')

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "hasLetters" $ nf hasLetters "cadevrsoi"     -- faster
    , bench "hasLetters'" $ nf hasLetters' "cadevrsoi"   -- slightly slower
    ]
  ]
