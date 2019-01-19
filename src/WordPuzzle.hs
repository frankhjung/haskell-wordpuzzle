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

import qualified Data.ByteString.Char8 as C (ByteString, cons, elem, empty,
                                             head, tail, uncons)

-- | Pattern for empty ByteString.
pattern Empty :: C.ByteString
pattern Empty <- (C.uncons -> Nothing)

-- | Remove first occurrence of a character from a word.
remove :: Char -> C.ByteString -> C.ByteString
remove _ Empty = C.empty
remove x ys = if x == h
                then t
                else h `C.cons` remove x t
              where
                h = C.head ys
                t = C.tail ys

-- | Check if a word contains only characters from a list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
isValid :: String -> C.ByteString -> Bool
isValid _  Empty  = True
isValid [] _      = False
isValid (x:xs) ys = if x `C.elem` ys
                      then isValid xs (x `remove` ys)
                      else isValid xs ys

