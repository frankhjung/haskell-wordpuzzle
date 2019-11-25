module Main(main) where

import           WordPuzzle            (isValid)

import qualified Data.ByteString.Char8 as Char8 (elem, length)
import           Data.Char             (isLetter, toLower)
import           Data.Semigroup        ((<>))
import           Data.Version          (showVersion)
import           Options.Applicative   (Parser, ParserInfo, ReadM, eitherReader,
                                        execParser, footer, fullDesc, header,
                                        help, helper, info, long, metavar,
                                        option, progDesc, short, showDefault,
                                        strOption, value, (<**>))
import           Paths_wordpuzzle      (version)
import           System.IO             (IOMode (ReadMode), withFile)
import qualified System.IO.Streams     as Streams (connect, filter,
                                                   handleToInputStream, lines,
                                                   stdout, unlines)

-- valid command line options
data Opts = Opts
              { _size       :: Int
              , _mandatory  :: Char
              , _letters    :: String
              , _dictionary :: FilePath
              }

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
-- Print words to stdout where:
--
-- 1. must be greater than the minimum word length
-- 2. must be no more than 9 characters long
-- 3. must contain mandatory character
-- 4. must contain only valid characters
-- 5. must not exceed valid character frequency

main :: IO ()
main = do
  (Opts size mandatory letters dictionary) <- execParser optsParser
  withFile dictionary ReadMode $ \handle -> do
    inWords <- Streams.handleToInputStream handle >>=
                Streams.lines >>=
                Streams.filter (\w -> size <= Char8.length w) >>=
                Streams.filter (\w -> Char8.length w <= 9) >>=
                Streams.filter (Char8.elem mandatory) >>=
                Streams.filter (isValid letters)
    outWords <- Streams.unlines Streams.stdout
    Streams.connect inWords outWords

