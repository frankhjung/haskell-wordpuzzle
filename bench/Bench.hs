module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (isWord)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "isWord" $ nf isWord "foobar"
    ]
  ]
