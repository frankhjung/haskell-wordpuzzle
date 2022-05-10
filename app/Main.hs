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
                                      checkSize, makeWordPuzzle, solve)

-- valid command line options
data Opts = Opts
              { size       :: Int
              , letters    :: String
              , dictionary :: FilePath
              } deriving (Show)

-- applicative structure for parser options
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
     <> help "Nine letters to make words"
     <> metavar "STRING" )
  <*> strOption
      ( long "dictionary"
     <> short 'd'
     <> help "Dictionary to search for words"
     <> showDefault
     <> value "dictionary"
     <> metavar "FILENAME" )

-- read size in range from 1 to 9
readerSize :: ReadM Int
readerSize = eitherReader readSizeOption

-- read an alphabetic string
readerLetters :: ReadM String
readerLetters = eitherReader checkLetters

-- read version from cabal configuration
packageVersion :: String
packageVersion = "Version: " <> showVersion version

-- parse arguments
optsParser :: ParserInfo Opts
optsParser = info (options <**> helper)
         ( header "https://github.com/frankhjung/haskell-wordpuzzle"
        <> fullDesc
        <> progDesc "Solve word puzzles like those at nineletterword.tompaton.com"
        <> footer packageVersion )

-- | Process size read from a command line option.
readSizeOption :: String            -- ^ string value to check as a valid size
               -> Either String Int -- ^ Left unexpected size or Right size
readSizeOption ss =
  let s = (readMaybe ss :: Maybe Int) in
  case s of
    Just i  -> checkSize i
    Nothing -> Left (show (UnexpectedValue ss))

--
-- MAIN
--
main :: IO ()
main = do
  opts <- execParser optsParser
  let wp = makeWordPuzzle (size opts) (letters opts) (dictionary opts)
  either print solve wp -- print error or show matching words

