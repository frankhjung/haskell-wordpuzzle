module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (isValid, remove)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "isValid" $ nf isValid "foobar"
    , bench "remove" $ nf remove 'a'
    ]
  ]
