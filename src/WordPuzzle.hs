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

module WordPuzzle ( remove
                  , isValid
                  ) where

import           Data.Bool             (bool)
import           Data.ByteString.Char8 (ByteString, cons, elem, empty, uncons)
import           Prelude               hiding (elem)

-- | Pattern for empty ByteString.
pattern Empty :: ByteString
pattern Empty <- (uncons -> Nothing)

-- | Remove first occurrence of a character from a word.
remove :: Char        -- ^ character to remove
       -> ByteString  -- ^ string to remove character from
       -> ByteString  -- ^ result string with one instance of character removed
remove _ Empty = empty
remove x ys = let Just (h, ts) = uncons ys
              in bool (h `cons` remove x ts) ts (x == h)

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

