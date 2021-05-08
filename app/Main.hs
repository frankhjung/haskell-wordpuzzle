module Main(main) where

import           WordPuzzle          (makeWordPuzzle, solve)

import           Data.Char           (isLetter, toLower)
import           Data.Semigroup      ((<>))
import           Data.Version        (showVersion)
import           Options.Applicative (Parser, ParserInfo, ReadM, eitherReader,
                                      execParser, footer, fullDesc, header,
                                      help, helper, info, long, metavar, option,
                                      progDesc, short, showDefault, strOption,
                                      value, (<**>))
import           Paths_wordpuzzle    (version)

-- valid command line options
data Opts = Opts
              { _size       :: Int
              , _mandatory  :: Char
              , _letters    :: String
              , _dictionary :: FilePath
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
  <*> option readerMandatory
      ( long "mandatory"
     <> short 'm'
     <> help "Mandatory character for all words"
     <> metavar "CHAR" )
  <*> option readerLetters
      ( long "letters"
     <> short 'l'
     <> help "Nine letters to make words"
     <> metavar "STRING" )
  <*> strOption
      ( long "dictionary"
     <> short 'd'
     <> help "Dictionary to read words from"
     <> showDefault
     <> value "dictionary"
     <> metavar "FILENAME" )

-- read size in range [1..9]
readerSize :: ReadM Int
readerSize = eitherReader readSize
  where
    readSize [] = Left "expected a number in range [1..9]"
    readSize ss = let s = read ss in
                  if s `elem` [1..9]
                  then Right s
                  else Left $ "unexpected size: " ++ ss

-- read an alphabetic character
readerMandatory :: ReadM Char
readerMandatory = eitherReader readMandatory
  where
    readMandatory [] = Left "expected 1 letter"
    readMandatory cs@(c:_) = if isLetter c
                             then Right $ toLower c
                             else Left $ "unexpected letter: " ++ cs

-- read an alphabetic string
readerLetters :: ReadM String
readerLetters = eitherReader readLetters
  where
    readLetters [] = Left "expected 9 letters"
    readLetters ls = if (9 == length ls) && all isLetter ls
                     then Right $ fmap toLower ls
                     else Left $ "unexpected letters: " ++ ls

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

--
-- MAIN
--
main :: IO ()
main = do
  opts <- execParser optsParser
  let wp = makeWordPuzzle (_size opts) (_mandatory opts) (_letters opts) (_dictionary opts)
  case wp of
    Left err   -> print err
    Right game -> solve game
