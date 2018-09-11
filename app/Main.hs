module Main(main) where

import           WordPuzzle          (filterWords, filterWords')

import           Data.Char           (isAlpha)
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, ReadM, auto,
                                      execParser, footer, fullDesc, header,
                                      help, helper, info, long, maybeReader,
                                      metavar, option, progDesc, short,
                                      showDefault, strOption, switch, value,
                                      (<**>))
import           System.Exit         (exitSuccess)

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
alpha = maybeReader $ \c -> if length c == 1 && isAlpha (head c)
                              then return $ head c
                              else Nothing

-- parse information
opts :: ParserInfo Opts
opts = info (options <**> helper)
         ( header "https://github.com/frankhjung/haskell-wordpuzzle"
        <> fullDesc
        <> progDesc "Solve word puzzles like those at nineletterword.tompaton.com"
        <> footer "Version: 0.4.1" )

-- print to screen all words matching criteria
showWords :: Opts -> IO ()
showWords (Opts size mandatory letters dictionary plurals) = do
  let checkWords = if plurals
                     then filterWords' size mandatory letters
                     else filterWords size mandatory letters
  dictionaryWords <- readFile dictionary
  mapM_ putStrLn $ filter checkWords (lines dictionaryWords)
  exitSuccess

--
-- MAIN
--
main :: IO ()
main = execParser opts >>= showWords
