{-|
  Module      : wordpuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  Supporting functions for solving letter word puzzles.
-}

module WordPuzzle (
                  -- * Supporting Functions
                    remove
                  , isValid
                  , isPlural
                  ) where

-- | Remove first occurrence of a character from the word.
--
-- c.f. http://hackage.haskell.org/package/base/docs/Data-List.html#v:delete
remove :: Char -> String -> String
remove _ []     = []
remove x (y:ys) = if x == y
                    then ys
                    else y : remove x ys

-- | Check if a word contains only characters from a list.
-- If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
-- If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
--
-- See also Data.List intersection function, '\\'
isValid :: String -> String -> Bool
isValid _  []     = True
isValid [] _      = False
isValid (x:xs) ys = if x `elem` ys
                      then isValid xs (x `remove` ys)
                      else isValid xs ys

-- | Check if word is a plural.
--
-- This assumes the word is at least 2 characters long.
--
-- (1) False if word does not end in 's'
-- 2. False if word ends in "ss"
-- 3. True Otherwise as ends in single 's'
--
isPlural :: String -> Bool
isPlural word
  | end /= 's'  = False  -- last character not an 's'
  | pen == "ss" = False  -- word does not end in "ss"
  | otherwise   = True
  where
    end = last word
    pen = drop (length word - 2) word

