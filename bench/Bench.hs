module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (isWord, removeLetter)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "isWord" $ nf isWord "foobar"
    , bench "removeLetter" $ nf removeLetter 'a'
    ]
  ]
