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

  TODO Validation returns a List of errors:

  > validateXyz :: a -> Validation [Error] a
-}

module WordPuzzle ( WordPuzzle
                  , makeWordPuzzle
                  , checkSize
                  , checkLetters
                  , checkMandatory
                  , isWord
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
                       | InvalidMandatory Char  -- ^ bad mandatory character
                       | InvalidLetters String  -- ^ bad letters
                       | UnexpectedValue String -- ^ couldn't parse value
                          deriving (Eq)

-- | Show 'ValidationError' as string.
instance Show ValidationError where
  show (InvalidSize n)      = "expected size in range [1..9], got " ++ show n
  show (InvalidMandatory m) = "invalid mandatory character, got " ++ [m]
  show (InvalidLetters ls)  = "expected 9 lowercase letters, got " ++ ls
  show (UnexpectedValue xs) = "unexpected value " ++ xs ++ " for parameter"

-- | Is size valid?
isSize :: Int -> Bool
isSize = inRange (1,9)

-- | Check that mandatory value is in the range from 1 to 9.
checkSize :: Int                 -- ^ value to check
            -> Either String Int -- ^ Left unexpected size or Right size
checkSize s = if isSize s
              then Right s
              else Left (show (InvalidSize s))

-- | Is mandatory letter valid?
isMandatory :: Char -> String -> Bool
isMandatory = elem

-- | Check that mandatory letter.
-- TODO Can we use isMandatory here? It will require an extra parameter,
-- which is not available by the parser.
checkMandatory :: String              -- ^ mandatory character to check
                -> Either String Char -- ^ valid mandatory letter
checkMandatory [] = Left (show (UnexpectedValue ""))
checkMandatory ms@(m:_)
  | 1 /= length ms    = Left (show (UnexpectedValue ms))
  | not (isLower m)   = Left (show (InvalidMandatory m))
  | otherwise         = Right m

-- | Are letters valid?
isLetters :: String -> Bool
isLetters ls = 9 == length ls && all isLower ls

-- | Check that letters are lowercase alphabetic characters.
checkLetters :: String                -- ^ characters to check
              -> Either String String -- ^ valid lowercase letters
checkLetters ls = if isLetters ls
                  then Right ls
                  else Left (show (InvalidLetters ls))

-- | Smart constructor for WordPuzzle.
-- TODO re-write using accumulative validation
-- See https://github.com/system-f/validation/blob/master/examples/src/Email.hs
makeWordPuzzle :: Int -> Char -> String -> FilePath -> Either ValidationError WordPuzzle
makeWordPuzzle s m ls d
  | not (isSize s)          = Left (InvalidSize s)
  | not (isMandatory m ls)  = Left (InvalidMandatory m)
  | not (isLetters ls)      = Left (InvalidLetters ls)
  | otherwise               = Right (WordPuzzle s m ls d)

-- | Solve word puzzle.
--
-- Print words to stdout.
solve :: WordPuzzle -> IO ()
solve puzzle = do
  dict <- readFile (dictionary puzzle)
  mapM_ putStrLn $ solve' puzzle (lines dict)

-- | Solve word puzzle given a dictionary of words.
-- 1. must be greater than the minimum word length
-- 2. must be no more than 9 characters long
-- 3. must contain mandatory character
-- 4. must contain only valid characters
-- 5. must not exceed valid character frequency
solve' :: WordPuzzle -> [String] -> [String]
solve' puzzle = filter (getPredicate (pS <> pM <> pL))
  where
    pS = Predicate (inRange (size puzzle, 9) . length)
    pM = Predicate (isMandatory (mandatory puzzle))
    pL = Predicate (isWord (letters puzzle))

-- | Check if a word contains only characters from a list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
isWord :: String      -- ^ valid letters
        -> String     -- ^ dictionary word to check
        -> Bool       -- ^ true if dictionary word matches letters
isWord _  []     = True
isWord [] _      = False
isWord (x:xs) ys = if x `elem` ys
                   then isWord xs (ys \\ [x])
                   else isWord xs ys
