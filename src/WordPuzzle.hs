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

module WordPuzzle ( delete
                  , filterPlurals
                  , filterWords
                  , isInValid
                  , isValid
                  ) where

-- * Helper Functions

-- | Delete first occurrence of the character in a list.
delete :: Char -> String -> String
delete _ []     = []
delete x (y:ys) = if x == y
                    then ys
                    else y : delete x ys

-- | Check if a word contains only characters from a list.
isValid :: String -> String -> Bool
isValid _  []     = True
isValid [] _      = False
isValid (x:xs) ys = if x `elem` ys
                      then isValid xs (x `delete` ys)
                      else isValid xs ys

-- | Opposite of 'isValid'.
isInValid :: String -> String -> Bool
isInValid x y = not (isValid x y)


-- * Filter Words Matching Criteria

-- | Only include words that match these rules:
--
-- (1) must be greater than the minimum word length
-- 2. must contain mandatory character
-- 3. must contain only valid characters
-- 4. must not exceed valid character frequency
--
filterWords :: Int -> Char -> String -> String -> Bool
filterWords s m xs ys
  | s > length ys   = False
  | m `notElem` ys  = False
  | isInValid xs ys = False
  | otherwise       = True


-- | Filter plurals that end in 's'
--
-- (1) Return true if word does not end in 's'
-- 2. Return false if 's' is truncated from word and that word is valid
--
filterPlurals :: String -> String -> Bool
filterPlurals xs ys
  | last ys /= 's'                       = True
  | isValid xs (take (length ys - 1) ys) = False
  | otherwise                            = True
