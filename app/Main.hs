{-|
  Executable  : wordpuzzle
  Description : Word Puzzle command line.
  Copyright   : © Frank Jung, 2017-2025
  License     : BSD-3-Clause
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}
module Main(main) where

import           Data.Version        (showVersion)
import           Options.Applicative (Parser, ParserInfo, ReadM, eitherReader,
                                      execParser, footer, fullDesc, header,
                                      help, helper, info, long, metavar, option,
                                      progDesc, short, showDefault, strOption,
                                      value, (<**>))
import           Paths_wordpuzzle    (version)
import           Text.Read           (readMaybe)
import           WordPuzzle          (ValidationError (..), checkLetters,
                                      checkSize, solve, toEither, validate)

-- | Valid command line options.
data Opts = Opts
              { size       :: Int       -- ^ Minimum word size
              , letters    :: String    -- ^ Letters to make words (4–9 characters)
              , dictionary :: FilePath  -- ^ Dictionary to search
              } deriving (Show)

-- | Applicative structure for parser command line options.
options :: Parser Opts
options = Opts
  <$> option readerSize
      ( long "size"
     <> short 's'
     <> help "Minimum word size (value from 1..9)"
     <> showDefault
     <> value 4
     <> metavar "INT" )
  <*> option readerLetters
      ( long "letters"
     <> short 'l'
     <> help "Letters to make words (4 to 9 unique lowercase letters)"
     <> metavar "STRING" )
  <*> strOption
      ( long "dictionary"
     <> short 'd'
     <> help "Dictionary to search for words"
     <> showDefault
     <> value "dictionary"
     <> metavar "FILENAME" )

-- | Read size in range from 1 to 9 (minimum word size).
readerSize :: ReadM Int -- ^ Size
readerSize = eitherReader readSizeOption

-- | Read an alphabetic string (letters of puzzle).
readerLetters :: ReadM String -- ^ from 4 to 9 letters to make words
readerLetters = eitherReader checkLetters

-- | Read application version from cabal configuration.
packageVersion :: String -- ^ Version string
packageVersion = "Version: " <> showVersion version

-- | Parse arguments.
optsParser :: ParserInfo Opts
optsParser = info (options <**> helper)
         ( header "https://github.com/frankhjung/haskell-wordpuzzle"
        <> fullDesc
        <> progDesc "Solve word puzzles"
        <> footer packageVersion )

-- | Process size read from a command line option.
readSizeOption :: String            -- ^ string value to check as a valid size
               -> Either String Int -- ^ Left unexpected size or Right size
readSizeOption ss =
  let s = (readMaybe ss :: Maybe Int) in
  case s of
    Just i  -> checkSize i
    Nothing -> Left (show (UnexpectedValue ss))

-- | Main.
-- Calls WordPuzzle.solve with options from command line.
main :: IO ()
main = do
  opts <- execParser optsParser
  let validation = validate (size opts) (letters opts) (dictionary opts)
  case toEither validation of
    Left errors  -> mapM_ print errors -- Print all validation errors
    Right puzzle -> solve puzzle
