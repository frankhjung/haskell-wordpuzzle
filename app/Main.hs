module Main(main) where

import qualified WordPuzzle         as WP (filterWords)

import qualified System.Environment as Env (getArgs, getProgName)
import qualified System.Exit        as Sys (exitFailure, exitSuccess)

-- Usage with current program name and command arguments
usage :: IO ()
usage = do
  progName <- Env.getProgName
  putStrLn $ "Usage: " ++ progName ++ " size mandatory letters [dictionary]"

-- Parse command line arguments
parseArgs :: [String] -> (Int, Char, String, FilePath)
parseArgs args = do
  let (s:m:l:d)  = args
      size       = read s :: Int
      mandatory  = head m :: Char
      letters    = l :: String
      dictionary = if not (null d)
                    then (head d :: FilePath)
                    else "dictionary" :: FilePath
  (size, mandatory, letters, dictionary)

-- Need either 3 or 4 parameters
isValid :: [String] -> Bool
isValid args
    | null args       = False
    | length args < 3 = False
    | length args > 4 = False
    | otherwise       = True

--
-- MAIN
--
main :: IO ()
main = do

  args <- Env.getArgs

  if not (isValid args)
    then do
      usage
      Sys.exitFailure
    else do
      let (size, mandatory, letters, dictionary) = parseArgs args
          checkWords = WP.filterWords size mandatory letters
      dictionaryWords <- readFile dictionary
      mapM_ putStrLn $ filter checkWords (lines dictionaryWords)
      Sys.exitSuccess
