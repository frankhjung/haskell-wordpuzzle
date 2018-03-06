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
                  , isInValid
                  , isValid
                  , isPlural
                  , filterWords
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

-- | Check if word is a plural.
--
-- (1) False if word does not end in 's'
-- 2. False if word ends in "ss"
-- 3. True Otherwise as ends in single 's'
--
isPlural :: String -> Bool
isPlural a
  | last a /= 's'          = False  -- last character not an 's'
  | last ( init a ) == 's' = False  -- last two characters not "ss"
  | otherwise              = True

-- * Filter Words Matching Criteria

-- | Only include words that match these rules:
--
-- (1) must be greater than the minimum word length
-- 2. must contain mandatory character
-- 3. must contain only valid characters
-- 4. must not exceed valid character frequency
-- 5. must not contain plurals (words ending in 's')
--
filterWords :: Int -> Char -> String -> String -> Bool
filterWords s m xs ys
  | s > length ys   = False
  | m `notElem` ys  = False
  | isInValid xs ys = False
  | isPlural ys     = False
  | otherwise       = True
