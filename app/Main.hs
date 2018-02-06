{-# LANGUAGE UnicodeSyntax #-}


module Main(main) where

-- import qualified Data.Text    as Text
-- import qualified Data.Text.IO as Text
import           Lib

main :: IO ()
main = do
    -- dictionary <- fmap Text.lines (Text.readFile "words")
    dictionary <- fmap lines (readFile "words")
    -- let p = filterWords 4 'c' "adevcrsoi"
    -- words <- filter p dictionary
    -- putStrLn $ "Found" ++ show (length words) ++ " valid words"
    putStrLn $ "From dictionary of " ++ show (length dictionary) ++ " words ..."
    -- print a sample of dictionary
    mapM_ putStrLn ( take 5 dictionary )
