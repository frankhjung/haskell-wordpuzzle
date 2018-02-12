{-# LANGUAGE UnicodeSyntax #-}


module Main(main) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Lib

main :: IO ()
main = do
    dictionaryWords <- fmap Text.lines (Text.readFile "words")
    putStrLn $ "loaded " ++ show (length dictionaryWords) ++ " words from dictionary"
