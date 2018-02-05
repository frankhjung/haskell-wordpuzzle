{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           Criterion.Main
import           Lib

main :: IO ()
main = defaultMain
  [ bgroup "isValid"
    [
      bench "foobar" $ nf isValid "foobar"
    ]
  ]

