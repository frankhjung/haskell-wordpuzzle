{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2019
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  Supporting functions for solving letter word puzzles.
-}

module WordPuzzle ( WordPuzzle
                  , solve
                  , isValid
                  , makeWordPuzzle
                  , remove
                  , ValidationError(..)
                  ) where

import           Data.Bool             (bool)
import           Data.ByteString.Char8 (ByteString, cons, elem, empty, length,
                                        uncons)
import           Data.Char             (isLower)
import           Data.Ix               (inRange)
import           Prelude               hiding (elem, length)
import           System.IO             (IOMode (ReadMode), withFile)
import qualified System.IO.Streams     as Streams (connect, filter,
                                                   handleToInputStream, lines,
                                                   stdout, unlines)

-- | Represent parameters required for the puzzle.
data WordPuzzle = WordPuzzle
                  {
                    size       :: Int
                  , mandatory  :: Char
                  , letters    :: String
                  , dictionary :: FilePath
                  } deriving (Show)

-- | Invalid parameter errors.
data ValidationError = InvalidSize Int | InvalidMandatory Char | InvalidLetters String
                          deriving (Show, Eq)

-- | Check if size is in the valid range from 1 to 9.
-- TODO - remove duplication of validation
-- isValidSize :: Char -> Either String Char
-- isValidSize s = if not (inRange (1,9) s) = Left (InvalidSize s)
-- if s `elem` [1..9]
--                   then Right s
--                   else Left $ "unexpected size: " ++ ss

-- | Smart constructor for WordPuzzle.
-- TODO - remove duplication of validation
makeWordPuzzle :: Int -> Char -> String -> FilePath -> Either ValidationError WordPuzzle
makeWordPuzzle s m ls d
  | not (inRange (1,9) s) = Left (InvalidSize s)
  | not (all isLower ls)  = Left (InvalidLetters ls)
  | m `notElem` ls        = Left (InvalidMandatory m)
  | otherwise             = Right (WordPuzzle s m ls d)

-- | Solve puzzle.
--
-- Print words to stdout where:
--
-- 1. must be greater than the minimum word length
-- 2. must be no more than 9 characters long
-- 3. must contain mandatory character
-- 4. must contain only valid characters
-- 5. must not exceed valid character frequency
solve :: WordPuzzle -> IO ()
solve puzzle =
  withFile (dictionary puzzle) ReadMode $ \handle -> do
    inWords <- Streams.handleToInputStream handle >>=
                Streams.lines >>=
                Streams.filter (\w -> size puzzle <= length w) >>=
                Streams.filter (\w -> length w <= 9) >>=
                Streams.filter (elem (mandatory puzzle)) >>=
                Streams.filter (isValid (letters puzzle))
    outWords <- Streams.unlines Streams.stdout
    Streams.connect inWords outWords

-- | Pattern for empty ByteString.
pattern Empty :: ByteString
pattern Empty <- (uncons -> Nothing)

-- | Check if a word contains only characters from a list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
isValid :: String     -- ^ valid letters
        -> ByteString -- ^ dictionary word to check
        -> Bool       -- ^ true if dictionary word matches letters
isValid _  Empty  = True
isValid [] _      = False
isValid (x:xs) ys = bool (isValid xs ys) (isValid xs (remove x ys)) (x `elem` ys)

-- | Remove first occurrence of a character from a word.
--
-- Used by 'isValid' to remove an element that is guaranteed to be present.
remove :: Char        -- ^ character to remove
       -> ByteString  -- ^ string to remove character from
       -> ByteString  -- ^ result string with one instance of character removed
remove _ Empty = empty
remove x ys = let Just (h, ts) = uncons ys
              in bool (h `cons` remove x ts) ts (x == h)
