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

  Validate functions return a 'Validation':

  > validateXyz :: a -> Validation [ValidationError] a
-}

module WordPuzzle ( WordPuzzle
                  , validateSize
                  , validateLetters
                  , nineLetters
                  , spellingBee
                  , solve
                  , ValidationError(..)
                  , validate
                  , toEither
                  ) where

import           Data.Bits                  ((.&.), (.|.), shiftL)
import qualified Data.ByteString.Char8      as BS
import           Data.Bool                  (bool)
import           Data.Char                  (isLower, ord)
import           Data.Functor.Contravariant (Predicate (..), getPredicate)
import           Data.Ix                    (inRange)
import           Data.List                  (nub)
import           Data.Validation            (Validation (..), toEither)
import qualified System.IO.Streams          as Streams

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
    deriving (Eq)

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

-- | Does word contain the mandatory letter?
hasMandatory :: Char -> BS.ByteString -> Bool
hasMandatory = BS.elem

-- | Solve word puzzle given a dictionary of words.
--
-- Where each word:
--
-- * must be greater than the minimum word length
-- * must contain mandatory character
-- * must contain only valid characters
-- * if repeats are not allowed, must not exceed valid character frequency
-- * when repeats are enabled there is no upper limit on word length; the word
--   may be longer than the letter pool itself.
--
-- Example:
--
-- solve (WordPuzzle 4 'a' "abcdefghij" "dictionary.txt")
solve :: WordPuzzle -> IO ()
solve wordpuzzle = Streams.withFileAsInput (dictionary wordpuzzle) $ \is -> do
  lines_is <- Streams.lines is
  filtered_is <- Streams.filter (getPredicate (pS <> pM <> pL)) lines_is
  consume filtered_is
  where
    pS = Predicate $ checkLength (repeats wordpuzzle) (size wordpuzzle)
    pM = Predicate $ hasMandatory (mandatory wordpuzzle)
    pL = Predicate $ checkLettersPool (repeats wordpuzzle) (letters wordpuzzle)
    consume is = do
      m <- Streams.read is
      case m of
        Nothing -> return ()
        Just x  -> BS.putStrLn x >> consume is

-- | Check word length based on whether repeats are allowed.
checkLength :: Bool -- ^ allow repeats?
            -> Int  -- ^ minimum word size
            -> BS.ByteString -- ^ word to check
            -> Bool -- ^ true if word length is valid
checkLength True  s = (>= s) . BS.length
checkLength False s = inRange (s, 9) . BS.length

-- | Check if a word matches the letter pool based on whether repeats are allowed.
--
-- Assumes that the letter pool is valid (see 'isLetters').
checkLettersPool :: Bool -- ^ allow repeats?
                  -> String -- ^ valid letters in letter pool
                  -> BS.ByteString -- ^ word to check
                  -> Bool -- ^ true if word matches letter pool
checkLettersPool True  ls = spellingBee ls
checkLettersPool False ls = nineLetters ls

-- | Check if a word contains only characters from a letters list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
--
-- Assumes that the letter pool is valid (see 'isLetters').
nineLetters ::
     String        -- ^ valid letters
  -> BS.ByteString -- ^ dictionary word to check
  -> Bool          -- ^ true if dictionary word matches letters
nineLetters ls ys = go (0 :: Int) ys
  where
    go !mask bs = case BS.uncons bs of
      Nothing -> True
      Just (c, rest) ->
        let bit = 1 `shiftL` (ord c - ord 'a')
        in (c `elem` ls) && (mask .&. bit == 0) && go (mask .|. bit) rest

-- | Check if a word contains only characters from a letters list.
-- Repeating characters are allowed.
--
-- Assumes that the letter pool is valid (see 'isLetters').
spellingBee ::
     String        -- ^ valid letters
  -> BS.ByteString -- ^ dictionary word to check
  -> Bool          -- ^ true if dictionary word matches letters
spellingBee ls ys = BS.all (`elem` ls) ys
