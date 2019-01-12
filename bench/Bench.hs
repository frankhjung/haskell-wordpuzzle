module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import           WordPuzzle     (filterWords, filterWords', isValid)

-- Exclude plurals
filterWordsEP = filterWords 4 'c'
-- Include plurals
filterWordsIP = filterWords' 4 'c'

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "isValid" $ nf isValid "foobar"
    , bench "filterWords" $ nf filterWordsEP "adevcrsoi"
    , bench "filterWords'" $ nf filterWordsIP "adevcrsoi"
    ]
  ]

