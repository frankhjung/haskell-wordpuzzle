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

module WordPuzzle ( WordPuzzle(..)
                  , solve
                  , isValid
                  , remove
                  ) where

import           Data.Bool             (bool)
import           Data.ByteString.Char8 (ByteString, cons, elem, empty, uncons)
import qualified Data.ByteString.Char8 as Char8 (elem, length)
import           Prelude               hiding (elem)
import           System.IO             (IOMode (ReadMode), withFile)
import qualified System.IO.Streams     as Streams (connect, filter,
                                                   handleToInputStream, lines,
                                                   stdout, unlines)

-- | Represent parameters required for the puzzle.
-- TODO create a smart constructor to validate game values.
data WordPuzzle = WordPuzzle
                  {
                    size       :: Int
                  , mandatory  :: Char
                  , letters    :: String
                  , dictionary :: FilePath
                  } deriving (Show)

-- | Solve puzzle.
solve :: WordPuzzle -> IO ()
solve puzzle =
  withFile (dictionary puzzle) ReadMode $ \handle -> do
    inWords <- Streams.handleToInputStream handle >>=
                Streams.lines >>=
                Streams.filter (\w -> size puzzle <= Char8.length w) >>=
                Streams.filter (\w -> Char8.length w <= 9) >>=
                Streams.filter (Char8.elem (mandatory puzzle)) >>=
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
