{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2025
  License     : BSD-3-Clause
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  Supporting functions for solving letter word puzzles.

  == Notes

  Is functions return a boolean:

  > isXyz :: a -> Bool

  Check functions return an 'Either':

  > checkXyz :: a -> Either Error a
-}

module WordPuzzle ( WordPuzzle
                  , checkSize
                  , checkLetters
                  , nineLetters
                  , spellingBee
                  , solve
                  , ValidationError(..)
                  , validate
                  , toEither
                  ) where

import           Data.Bool                  (bool)
import           Data.Char                  (isLower)
import           Data.Functor.Contravariant (Predicate (..), getPredicate)
import           Data.Ix                    (inRange)
import           Data.List                  (delete, nub)
import           Data.Validation            (Validation (..), toEither)

-- | Represent parameters required for the puzzle.
data WordPuzzle = WordPuzzle
                  {
                    size       :: Int      -- ^ minimum size of words (must be between 4 and 9)
                  , mandatory  :: Char     -- ^ mandatory character in word
                  , letters    :: String   -- ^ letters to make words (4–9 unique lowercase characters)
                  , dictionary :: FilePath -- ^ dictionary for valid words
                  , repeats    :: Bool     -- ^ whether letters can be repeated
                  } deriving (Show)

-- | Error given on invalid parameter.
data ValidationError =
    InvalidSize (Int, Int) Int      -- ^ expected range and actual size
    | InvalidLetters String         -- ^ actual letters (should be 4-9 unique lowercase letters)
    | UnexpectedValue String        -- ^ couldn't parse value

-- | Show 'ValidationError' as string.
instance Show ValidationError where
  show (InvalidSize (en1,en2) an)  = "expected value in range ("
    ++ show en1 ++ ", " ++ show en2 ++ ") got " ++ show an
  show (InvalidLetters ls)  = "expected 4-9 unique lowercase letters, got " ++ ls
  show (UnexpectedValue xs) = "unexpected value " ++ xs ++ " for parameter"

-- | Validate program parameters.
validate :: Bool -> Int -> String -> FilePath -> Validation [ValidationError] WordPuzzle
validate _ _ [] _ = Failure [InvalidLetters "empty letters"]
validate r s (m:ls) d =
  WordPuzzle <$> validateSize s         -- validate size
             <*> pure m                 -- mandatory letter
             <*> validateLetters (m:ls) -- validate letters
             <*> pure d                 -- dictionary
             <*> pure r                 -- repeat letters

-- | Validate size of word.
validateSize :: Int -> Validation [ValidationError] Int
validateSize s = bool (Failure [InvalidSize (4,9) s]) (Success s) (isSize s)


-- | Validate letters.
validateLetters :: String -> Validation [ValidationError] String
validateLetters ls = bool (Failure [InvalidLetters ls]) (Success ls) (isLetters ls)

-- | Is size valid?  The value must be between 4 and 9 inclusive.
--
-- >>> isSize 9
-- True
--
-- >>> isSize 10
-- False
--
-- >>> isSize 3
-- False
isSize :: Int -> Bool
isSize = inRange (4,9)

-- | Check that mandatory value is in the range from 4 to 9.
--
-- >>> checkSize 10
-- Left (InvalidSize 10)
--
-- >>> checkSize 1
-- Left (InvalidSize 1)
--
-- >>> checkSize 4
-- Right 4
checkSize :: Int                 -- ^ size of word to check
            -> Either String Int -- ^ Left unexpected size or Right size
checkSize s = bool (Left (show (InvalidSize (4,9) s))) (Right s) (isSize s)

-- | Are letters valid?  Valid strings contain between 4 and 9
-- *unique* lowercase letters.
--
-- >>> isLetters "abcd"
-- True
--
-- >>> isLetters "abca"
-- False -- repeated character
--
-- >>> isLetters "abcdefghij"
-- False  -- too long
--
-- >>> isLetters "abcDefg"
-- False -- mixed case
isLetters :: String -> Bool
isLetters ls =
  inRange (4,9) n && all isLower ls && length (nub ls) == n
  where
    n = length ls

-- | Check that letters are lowercase alphabetic characters.
checkLetters :: String                -- ^ characters to check
              -> Either String String -- ^ valid lowercase letters
checkLetters ls = bool (Left (show (InvalidLetters ls))) (Right ls) (isLetters ls)

-- | Does word contain the mandatory letter?
hasMandatory :: Char -> String -> Bool
hasMandatory = elem

-- | Solve word puzzle given a dictionary of words.
--
-- Where each word:
--
-- * must be greater than the minimum word length
-- * must be no more than 9 characters long
-- * must contain mandatory character
-- * must contain only valid characters
-- * must not exceed valid character frequency
--
-- Example:
--
-- solve (WordPuzzle 4 'a' "abcdefghij" "dictionary.txt")
solve :: WordPuzzle -> IO ()
solve wordpuzzle = do
  dict <- readFile (dictionary wordpuzzle)
  mapM_ putStrLn $ go wordpuzzle (lines dict)
  where
    go :: WordPuzzle -> [String] -> [String]
    go puzzle = filter (getPredicate (pS <> pM <> pL))
      where
        pS = Predicate (if repeats puzzle
                         then (>= size puzzle) . length
                         else inRange (size puzzle, 9) . length)
        pM = Predicate (hasMandatory (mandatory puzzle))
        -- ensure the supplied pool of letters itself is valid; if not, no
        -- dictionary words should match the puzzle.
        validLetters = isLetters (letters puzzle)
        pL = Predicate $ if validLetters
                         then if repeats puzzle
                              then spellingBee (letters puzzle)
                              else nineLetters (letters puzzle)
                         else const False

-- | Check if a word contains only characters from a letters list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
nineLetters ::
     String     -- ^ valid letters
  -> String     -- ^ dictionary word to check
  -> Bool       -- ^ true if dictionary word matches letters
nineLetters ls ys = isLetters ls && go ls ys
  where
    go _  []      = True -- if all characters are removed, the word is valid
    go [] _       = False -- if there are still characters left over, the word is not valid
    go (x:xs) ys' = go xs (delete x ys') -- if the character is valid, remove it from the word and continue checking

-- | Check if a word contains only characters from a letters list.
-- Repeating characters are allowed.
spellingBee ::
     String     -- ^ valid letters
  -> String     -- ^ dictionary word to check
  -> Bool       -- ^ true if dictionary word matches letters
spellingBee ls ys = isLetters ls && go ls ys
  where
    go _ []     = True -- if all characters are removed, the word is valid
    go ls' (y:ys') -- guard against invalid letter pools
      | y `elem` ls' = go ls' ys' -- if the character is valid, remove it from the word and continue checking
      | otherwise    = False -- if the character is not valid, the word does not match
