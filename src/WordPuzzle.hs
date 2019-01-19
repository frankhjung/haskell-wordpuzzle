{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-|
  Module      : wordpuzzle
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

import qualified Data.ByteString.Char8 as Char8 (ByteString, cons, elem, empty,
                                                 head, tail, uncons)

-- | Pattern for empty ByteString.
pattern Empty :: Char8.ByteString
pattern Empty <- (Char8.uncons -> Nothing)

-- | Remove first occurrence of a character from a word.
remove :: Char -> Char8.ByteString -> Char8.ByteString
remove _ Empty = Char8.empty
remove x ys = if x == h
                then t
                else h `Char8.cons` remove x t
              where
                h = Char8.head ys
                t = Char8.tail ys

-- | Check if a word contains only characters from a list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
isValid :: String -> Char8.ByteString -> Bool
isValid _  Empty  = True
isValid [] _      = False
isValid (x:xs) ys = if x `Char8.elem` ys
                      then isValid xs (x `remove` ys)
                      else isValid xs ys

