{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           Criterion.Main
import           Lib

main :: IO ()
main = defaultMain
  [ bgroup "myGreeting"
    [
      bench "greet" $ whnf greet "Hello\tWorld"
    , bench "greet" $ whnf greet "Greetings Earthlings!\n"
    ]
  ]

