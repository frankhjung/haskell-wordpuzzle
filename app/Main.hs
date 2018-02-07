{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import qualified Lib                (filterWords)

import qualified System.Environment as Env (getArgs, getProgName)
import qualified System.Exit        (exitFailure, exitSuccess)

main :: IO ()
main = do

  args <- Env.getArgs

  if null args
    then do
      progName <- Env.getProgName
      putStrLn $ "Usage: " ++ progName ++ " size mandatory letters [dictionary]"
      System.Exit.exitFailure
    else do
      -- parse arguments
      let (s:m:l:d) = args
          size = read s :: Int
          mandatory = head m :: Char
          letters = l :: String
          dictionary = if not (null d)
                        then (head d :: FilePath)
                        else "dictionary" :: FilePath
      -- show arguments
      showArgs size mandatory letters dictionary

      let p = Lib.filterWords size mandatory letters
      dictionaryWords <- readFile dictionary
      mapM_ putStrLn $
        filter p (lines dictionaryWords)

      System.Exit.exitSuccess

showArgs :: Int -> Char -> String -> FilePath -> IO ()
showArgs size mandatory letters dictionary = do
  putStrLn $ "Words from dictionary: " ++ show dictionary
  putStrLn $ "Word minimum size: " ++ show size
  putStrLn $ "Words must contain: " ++ show mandatory
  putStrLn $ "Words are made from letters: " ++ letters
  putStrLn   "Words found are:"

