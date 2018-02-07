{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           Criterion.Main
import           Lib            (filterWords, isValid)

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

