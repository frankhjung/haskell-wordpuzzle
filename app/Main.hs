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
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      footer, fullDesc, header, help, helper,
                                      info, long, metavar, option, progDesc,
                                      short, showDefault, strOption, switch,
                                      value, (<**>))
import           Paths_wordpuzzle    (version)
import           WordPuzzle          (solve, toEither, validate)

-- | Valid command line options.
data Opts = Opts
              { size       :: Int       -- ^ Minimum word size (4-9 characters)
              , letters    :: String    -- ^ Letters to make words (4-9 characters)
              , dictionary :: FilePath  -- ^ Dictionary to search
              , repeats    :: Bool      -- ^ Allow letters to repeat
              } deriving (Show)

-- | Applicative structure for parser command line options.
options :: Parser Opts
options = Opts
  <$> option auto
      ( long "size"
     <> short 's'
     <> help "Minimum word size is (4-9)"
     <> showDefault
     <> value 4
     <> metavar "INT" )
  <*> strOption
      ( long "letters"
     <> short 'l'
     <> help "4-9 unique lowercase letters to make words"
     <> metavar "STRING" )
  <*> strOption
      ( long "dictionary"
     <> short 'd'
     <> help "Dictionary to search for words"
     <> showDefault
     <> value "dictionary"
     <> metavar "FILENAME" )
  <*> switch
      ( long "repeats"
     <> short 'r'
     <> help "Allow letters to repeat (like Spelling Bee)" )

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

-- | Main.
-- Calls WordPuzzle.solve with options from command line.
main :: IO ()
main = do
  opts <- execParser optsParser
  let validation = validate (repeats opts) (size opts) (letters opts) (dictionary opts)
  case toEither validation of
    Left errors  -> mapM_ print errors -- Print all validation errors
    Right puzzle -> solve puzzle
