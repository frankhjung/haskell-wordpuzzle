{-# LANGUAGE UnicodeSyntax #-}

{-|

  Module      : greet
  Description : Print a greeting to console.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : experimental
  Portability : Linux

  A simple program to demonstrate project structure using the following
  [Haskell](https://www.haskell.org/) tools:

  * [Cabal](https://www.haskell.org/cabal/)
  * [Criterion.Main](https://hackage.haskell.org/package/criterion)
  * [Haddock](https://www.haskell.org/haddock/)
  * [hlint](https://hackage.haskell.org/package/hlint)
  * [Stack](https://www.haskellstack.org/)
  * [Stylish Haskell](https://hackage.haskell.org/package/stylish-haskell-0.8.1.0)
  * [Test.Hspec](https://hackage.haskell.org/package/hspec)

-}

module Lib (greet) where

-- | The greet function is an alias to "putStrLn".

-- | Greet will print a message to the console.
greet :: String -> IO ()
greet = putStrLn
