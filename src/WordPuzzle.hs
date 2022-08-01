{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2021
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

  TODO Update validation to return a List of errors:

  > validateXyz :: a -> Validation [Error] a
-}

module WordPuzzle ( WordPuzzle
                  , makeWordPuzzle
                  , checkSize
                  , checkLetters
                  , hasLetters
                  , solve
                  , ValidationError(..)
                  ) where

import           Data.Char                  (isLower)
import           Data.Functor.Contravariant (Predicate (..), getPredicate)
import           Data.Ix                    (inRange)
import           Data.List                  ((\\))

-- | Represent parameters required for the puzzle.
data WordPuzzle = WordPuzzle
                  {
                    size       :: Int      -- ^ minimum size of words
                  , mandatory  :: Char     -- ^ mandatory character in word
                  , letters    :: String   -- ^ letters to make words
                  , dictionary :: FilePath -- ^ dictionary for valid words
                  } deriving (Show)

-- | Error given on invalid parameter.
data ValidationError = InvalidSize Int          -- ^ bad size integer
                       | InvalidLetters String  -- ^ bad letters
                       | UnexpectedValue String -- ^ couldn't parse value

-- | Show 'ValidationError' as string.
instance Show ValidationError where
  show (InvalidSize n)      = "expected size in range [1..9], got " ++ show n
  show (InvalidLetters ls)  = "expected 9 lowercase letters, got " ++ ls
  show (UnexpectedValue xs) = "unexpected value " ++ xs ++ " for parameter"

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
checkSize :: Int                 -- ^ value to check
            -> Either String Int -- ^ Left unexpected size or Right size
checkSize s = if isSize s
              then Right s
              else Left (show (InvalidSize s))

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
checkLetters ls = if isLetters ls
                  then Right ls
                  else Left (show (InvalidLetters ls))

-- | Does word contain the mandatory letter?
hasMandatory :: Char -> String -> Bool
hasMandatory = elem

-- | Smart constructor for WordPuzzle.
--
-- TODO re-write using accumulative validation
--
-- See https://github.com/system-f/validation/blob/master/examples/src/Email.hs
makeWordPuzzle :: Int -> String -> FilePath -> Either ValidationError WordPuzzle
makeWordPuzzle s ls d
  | not (isSize s)          = Left (InvalidSize s)
  | not (isLetters ls)      = Left (InvalidLetters ls)
  | otherwise               = Right (WordPuzzle s m ls d)
  where m = head ls  -- valid as ls already checked

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
hasLetters :: String     -- ^ valid letters
           -> String     -- ^ dictionary word to check
           -> Bool       -- ^ true if dictionary word matches letters
hasLetters _  []     = True
hasLetters [] _      = False
hasLetters (x:xs) ys = hasLetters xs (ys \\ [x])
