{-# LANGUAGE UnicodeSyntax #-}


module Main(main) where

import           Lib

main :: IO ()
main = do
  dictionary <- readFile "words"
  let p = filterWords 4 'c' "adevcrsoi"
  mapM_ putStrLn $
    filter p (lines dictionary)
