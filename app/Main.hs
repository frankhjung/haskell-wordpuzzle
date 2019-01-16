module Main(main) where

import           WordPuzzle            (isPlural, isValid)

import qualified Data.ByteString.Char8 as Char8 (elem, length, unpack)
import           Data.Char             (isAlpha)
import           Data.Semigroup        ((<>))
import           Options.Applicative   (Parser, ParserInfo, ReadM, auto,
                                        execParser, footer, fullDesc, header,
                                        help, helper, info, long, maybeReader,
                                        metavar, option, progDesc, short,
                                        showDefault, strOption, switch, value,
                                        (<**>))
import           System.IO             (IOMode (ReadMode), withFile)
import qualified System.IO.Streams     as Streams (connect, filter,
                                                   handleToInputStream, lines,
                                                   stdout, unlines)

-- command line options
data Opts = Opts
              { _size       :: Int
              , _mandatory  :: Char
              , _letters    :: String
              , _dictionary :: FilePath
              , _plurals    :: Bool
              }

-- structure for parser
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
  <*> switch
      ( long "plurals"
     <> short 'p'
     <> help "Include plural words" )

-- custom reader of char rather than string
alpha :: ReadM Char
alpha = maybeReader $ \c ->
  if Prelude.length c == 1 && isAlpha (head c)
    then return $ head c
    else Nothing

-- parse information
opts :: ParserInfo Opts
opts = info (options <**> helper)
         ( header "https://github.com/frankhjung/haskell-wordpuzzle"
        <> fullDesc
        <> progDesc "Solve word puzzles like those at nineletterword.tompaton.com"
        <> footer "Version: 0.5.1" )

-- Print words to stdout where:
--
-- 1. must be greater than the minimum word length
-- 2. must contain mandatory character
-- 3. must contain only valid characters
-- 4. must not exceed valid character frequency
-- 5. (optional) exclude plurals
showValidWords :: Opts -> IO ()
showValidWords (Opts size mandatory letters dictionary plurals) =
  withFile dictionary ReadMode $ \handle -> do
    inWords <- Streams.handleToInputStream handle >>=
                Streams.lines >>=
                Streams.filter (\w -> size <= Char8.length w) >>=
                Streams.filter (Char8.elem mandatory) >>=
                Streams.filter (isValid letters . Char8.unpack) >>=
                Streams.filter (\w -> plurals || isPlural (Char8.unpack w))
    outWords <- Streams.unlines Streams.stdout
    Streams.connect inWords outWords

--
-- MAIN
--
main :: IO ()
main = execParser opts >>= showValidWords
