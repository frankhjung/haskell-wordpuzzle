{-# LANGUAGE UnicodeSyntax #-}


module Main(main) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Lib

main :: IO ()
main = do
    dictionary <- fmap Text.lines (Text.readFile "words")
    putStrLn $ "loaded " ++ show (length dictionary) ++ " words from dictionary"
