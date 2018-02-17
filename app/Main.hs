module Main(main) where

import           WordPuzzle            (filterWords)

import           Data.Char             (isAlpha)
import           Data.Maybe            (fromJust, fromMaybe)
import           System.Console.GetOpt (ArgDescr (NoArg, OptArg, ReqArg),
                                        ArgOrder (RequireOrder),
                                        OptDescr (Option), getOpt, usageInfo)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (hPutStrLn, stderr)

-- Command line flags
data Options = Options { optHelp       :: Bool
                       , optSize       :: Int
                       , optMandatory  :: Char
                       , optLetters    :: String
                       , optDictionary :: Maybe FilePath
                       } deriving Show

-- Option defaults
startOptions :: Options
startOptions = Options { optHelp = False
                       , optSize = 4
                       , optMandatory = ' '
                       , optLetters = []
                       , optDictionary = Just "dictionary"
                       }

-- Command line options
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "s" ["size"]
      (ReqArg
        (\arg opt -> return opt { optSize = read arg })
        "int"
      )
      "Minimum word size"

  , Option "m" ["mandatory"]
      (ReqArg
        (\arg opt -> return opt { optMandatory = head arg })
        "char"
      )
      "Mandatory character for all words"

  , Option "l" ["letters"]
      (ReqArg
        (\arg opt -> return opt { optLetters = arg })
        "string"
      )
      "String of letters to make words"

  , Option "d" ["dictionary"]
      (OptArg
        ((\f opt -> return opt { optDictionary = Just f }) . fromMaybe "dictionary")
        "FILE"
      )
      "Path of alternate dictionary"

  , Option "h" ["help"]
      (NoArg
        (\_ -> do
          progName <- getProgName
          hPutStrLn stderr (usageInfo progName options)
          exitSuccess
        )
      )
      "Show help"
  ]


--
-- MAIN
--
main :: IO ()
main = do

  args <- getArgs

  -- parse options, getting a list of parameters
  let (parameters, nonOptions, errors) = getOpt RequireOrder options args

  -- add defaults to option parameters not parsed
  opts <- foldl (>>=) (return startOptions) parameters

  -- map options to local variables
  let Options { optHelp = _
              , optSize = size
              , optMandatory = mandatory
              , optLetters = letters
              , optDictionary = dictionary
              } = opts

  -- check for parameter errors, if none then solve wordpuzzle
  if not (isAlpha mandatory) || not (null errors) || not (null nonOptions) || null letters
    then do
      progName <- getProgName
      hPutStrLn stderr (concat errors ++ usageInfo progName options)
      exitFailure
      -- the same bu using bind (no need for do)
      -- getProgName >>= \progName -> hPutStrLn stderr (concat errors ++ usageInfo progName options) >> exitFailure
    else do
      let checkWords = filterWords size mandatory letters
      dictionaryWords <- readFile (fromJust dictionary)
      mapM_ putStrLn $ filter checkWords (lines dictionaryWords)
      exitSuccess
