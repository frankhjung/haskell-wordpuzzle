{-# LANGUAGE UnicodeSyntax #-}

{-|

  Module      : wordpuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : experimental
  Portability : Linux

  Supporting functions for 9 letter word puzzles.

-}

module Lib ( delete
           , filterWords
           , isValid
           ) where

-- | Check if the word contains valid characters from a list.
--
-- The character freqency in the word can not exceed the frequency in the
-- list.
isValid :: String -> String -> Bool
isValid _  []     = True
isValid [] _      = False
isValid (x:xs) ys = if x `elem` ys
                      then isValid xs (x `delete` ys)
                      else isValid xs ys

-- | Delete first occurrence of the character in a list.
delete :: Char -> String -> String
delete _ []     = []
delete x (y:ys) = if x == y
                    then ys
                    else y : delete x ys

-- | Filter words that match rules:
--   * must be greater than the minimum length word length
--   * must contain mandatory character
--   * must contain only valid characters
--   * must not exceed valid character frequency
filterWords :: Int -> Char -> String -> String -> Bool
filterWords s m xs ys
  | s > length ys = False
  | m `notElem` ys = False
  | not (isValid xs ys) = False
  | otherwise = True

