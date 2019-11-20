module Main(main) where

import           WordPuzzle            (isValid)

import qualified Data.ByteString.Char8 as Char8 (elem, length)
import           Data.Semigroup        ((<>))
import           Data.Version          (showVersion)
import           Options.Applicative   (Parser, ParserInfo, ReadM, auto,
                                        execParser, footer, fullDesc, header,
                                        help, helper, info, long, maybeReader,
                                        metavar, option, progDesc, short,
                                        showDefault, strOption, value, (<**>))
import           Paths_wordpuzzle      (version)
import           System.Exit           (exitSuccess)
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
  <$> option auto
      ( long "size"
     <> short 's'
     <> help "Minimum word size"
     <> showDefault
     <> value 4
     <> metavar "INT" )
  <*> option alpha
      ( long "mandatory"
     <> short 'm'
     <> help "Mandatory character for all words"
     <> metavar "CHAR" )
  <*> strOption
      ( long "letters"
     <> short 'l'
     <> help "String of letters to make words"
     <> metavar "STRING" )
  <*> strOption
      ( long "dictionary"
     <> short 'd'
     <> help "Alternate dictionary"
     <> showDefault
     <> value "dictionary"
     <> metavar "FILENAME" )

-- get first character of a string
firstLetter :: String -> Maybe Char
firstLetter (c:_) = Just c
firstLetter _     = Nothing

-- custom reader of char rather than string
alpha :: ReadM Char
alpha = maybeReader firstLetter

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
  exitSuccess

