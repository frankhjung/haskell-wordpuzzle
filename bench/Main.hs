module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (filterWords, isValid)

filterWords' = filterWords 4 'c'

main :: IO ()
main = defaultMain
  [ bgroup "isValid"
    [
      bench "foobar" $ nf isValid "foobar"
    ],
    bgroup "filterWords"
    [
      bench "filterWords" $ nf filterWords' "adevcrsoi"
    ]
  ]

