{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2025
  License     : GPL-3
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
                  , hasLetters
                  , hasLetters'
                  , solve
                  , ValidationError(..)
                  , validate
                  , toEither
                  ) where

import           Data.Bool                  (bool)
import           Data.Char                  (isLower)
import           Data.Functor.Contravariant (Predicate (..), getPredicate)
import           Data.Ix                    (inRange)
import           Data.List                  (delete)
import           Data.Validation            (Validation (..), toEither)

-- | Represent parameters required for the puzzle.
data WordPuzzle = WordPuzzle
                  {
                    size       :: Int      -- ^ minimum size of words
                  , mandatory  :: Char     -- ^ mandatory character in word
                  , letters    :: String   -- ^ letters to make words
                  , dictionary :: FilePath -- ^ dictionary for valid words
                  } deriving (Show)

-- | Error given on invalid parameter.
data ValidationError =
    InvalidSize (Int, Int) Int      -- ^ expected range and actual size
    | InvalidLetters String         -- ^ actual letters
    | UnexpectedValue String        -- ^ couldn't parse value

-- | Show 'ValidationError' as string.
instance Show ValidationError where
  show (InvalidSize (en1,en2) an)  = "expected value in range (" ++ show en1 ++ ", " ++ show en2 ++ ") got " ++ show an
  show (InvalidLetters ls)  = "expected lowercase letters, got " ++ ls
  show (UnexpectedValue xs) = "unexpected value " ++ xs ++ " for parameter"

-- | Validate program parameters.
validate :: Int -> String -> FilePath -> Validation [ValidationError] WordPuzzle
validate _ [] _ = Failure [InvalidLetters "empty letters"]
validate s (m:ls) d =
  WordPuzzle <$> validateSize s         -- validate size
             <*> pure m                 -- mandatory letter
             <*> validateLetters (m:ls) -- validate letters
             <*> pure d                 -- dictionary

-- | Validate size of word.
validateSize :: Int -> Validation [ValidationError] Int
validateSize s = bool (Failure [InvalidSize (1,9) s]) (Success s) (isSize s)


-- | Validate letters.
validateLetters :: String -> Validation [ValidationError] String
validateLetters ls = bool (Failure [InvalidLetters ls]) (Success ls) (isLetters ls)

-- | Is size valid?
--
-- >>> isSize 9
-- True
--
-- >>> isSize 10
-- False
isSize :: Int -> Bool
isSize = inRange (1,9)

-- | Check that mandatory value is in the range from 1 to 9.
--
-- >>> checkSize 10
-- Left (InvalidSize 10)
--
-- >>> checkSize 1
-- Right 1
checkSize :: Int                 -- ^ size of word to check
            -> Either String Int -- ^ Left unexpected size or Right size
checkSize s = bool (Left (show (InvalidSize (1,9) s))) (Right s) (isSize s)

-- | Are letters valid?
--
-- >>> isLetters "abcdefghij"
-- True
--
-- >>> isLetters "abcDefghij"
-- False
isLetters :: String -> Bool
isLetters ls = 9 == length ls && all isLower ls

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
solve :: WordPuzzle -> IO ()
solve wordpuzzle = do
  dict <- readFile (dictionary wordpuzzle)
  mapM_ putStrLn $ go wordpuzzle (lines dict)
  where
    go :: WordPuzzle -> [String] -> [String]
    go puzzle = filter (getPredicate (pS <> pM <> pL))
      where
        pS = Predicate (inRange (size puzzle, 9) . length)
        pM = Predicate (hasMandatory (mandatory puzzle))
        pL = Predicate (hasLetters (letters puzzle))

-- | Check if a word contains only characters from a letters list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
hasLetters ::
     String     -- ^ valid letters
  -> String     -- ^ dictionary word to check
  -> Bool       -- ^ true if dictionary word matches letters
hasLetters _  []     = True
hasLetters [] _      = False
hasLetters (x:xs) ys = hasLetters xs (delete x ys)


-- | Check if a word contains only characters from a letters list.
--
-- Original version using set difference.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
hasLetters' :: String    -- ^ valid letters
            -> String    -- ^ dictionary word to check
            -> Bool      -- ^ true if dictionary word matches letters
hasLetters' _  []     = True
hasLetters' [] _      = False
hasLetters' xs (y:ys)
  | y `elem` xs = hasLetters' (delete y xs) ys
  | otherwise   = False
