{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2026
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
                  , mkWordPuzzle
                  , size, mandatory, letters
                  , dictionary, repeats
                  , poolMask
                  , maskOf
                  , validateSize
                  , validateLetters
                  , nineLetters
                  , spellingBee
                  , solve
                  , solver
                  , ValidationError(..)
                  , toEither
                  ) where

import           Data.Bits                  (unsafeShiftL, (.&.), (.|.))
import           Data.Bool                  (bool)
import qualified Data.ByteString.Char8      as BS
import           Data.Char                  (isLower, ord)
import           Data.Functor.Contravariant (Predicate (..), getPredicate)
import           Data.Ix                    (inRange)
import           Data.List                  (foldl', nub)
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
                  , poolMask   :: !Int     -- ^ bitmask of 'letters' (see 'maskOf')
                  } deriving (Eq, Show)

-- | Error given on invalid parameter.
data ValidationError =
    InvalidSize (Int, Int) Int      -- ^ expected range and actual size
    | InvalidLetters String         -- ^ actual letters (should be 4-9 unique lowercase letters)
    | UnexpectedValue String        -- ^ couldn't parse value
    | InvalidMandatory Char         -- ^ mandatory character not lowercase a-z
    | MandatoryNotInLetters Char String -- ^ mandatory character not in letters
    deriving (Eq)

-- | Show 'ValidationError' as string.
instance Show ValidationError where
  show (InvalidSize (en1,en2) an)  = "expected value in range ("
    ++ show en1 ++ ", " ++ show en2 ++ ") got " ++ show an
  show (InvalidLetters ls)  = "expected 4-9 unique lowercase letters, got " ++ ls
  show (UnexpectedValue xs) = "unexpected value " ++ xs ++ " for parameter"
  show (InvalidMandatory c) = "expected lowercase letter for mandatory, got " ++ show c
  show (MandatoryNotInLetters c ls) = "mandatory letter " ++ show c ++ " not in letters " ++ show ls

-- | Smart constructor for 'WordPuzzle'.
--
-- The data constructor is not exported from this module.
-- Use this function to create a validated 'WordPuzzle'.
--
-- The letter pool bitmask ('poolMask') is computed once here, so it is not
-- rebuilt for every dictionary word.
mkWordPuzzle :: Bool -> Int -> Char -> String -> FilePath
             -> Validation [ValidationError] WordPuzzle
mkWordPuzzle r s m ls d =
  WordPuzzle <$> validateSize s
             <*> validateMandatory m
             <*> validateLetters ls
             <*> pure d
             <*> pure r
             <*> pure (maskOf ls)
             <* validateMandatoryInLetters m ls

-- | Validate mandatory character.
validateMandatory :: Char -> Validation [ValidationError] Char
validateMandatory m = bool (Failure [InvalidMandatory m]) (Success m) (isLower m)

-- | Validate mandatory character is in letters.
validateMandatoryInLetters :: Char -> String -> Validation [ValidationError] ()
validateMandatoryInLetters m ls = bool (Failure [MandatoryNotInLetters m ls]) (Success ()) (isLower m && m `elem` ls)

-- | Validate size of word.
validateSize :: Int -> Validation [ValidationError] Int
validateSize s = bool (Failure [InvalidSize (4,9) s]) (Success s) (isSize s)

-- | Validate letters.
validateLetters :: String -> Validation [ValidationError] String
validateLetters "" = Failure [InvalidLetters "empty letters"]
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
-- @
-- solve (WordPuzzle 4 'a' "abcdefghij" "dictionary.txt")
-- @
--
-- Method:
--
-- * open file
-- * split into lines
-- * filter
-- * print each
-- * force traversal to EOF
solve :: WordPuzzle -> IO ()
solve wordpuzzle = Streams.withFileAsInput (dictionary wordpuzzle) $ \is -> do
  lines_is <- Streams.lines is
  filtered_is <- solver wordpuzzle lines_is
  printed_is <- Streams.mapM_ BS.putStrLn filtered_is
  Streams.skipToEof printed_is

-- | Filter words from an input stream based on the puzzle constraints.
solver :: WordPuzzle -> Streams.InputStream BS.ByteString -> IO (Streams.InputStream BS.ByteString)
solver puzzle = Streams.filter (getPredicate (pS <> pM <> pL))
  where
    pS = Predicate $ checkLength (repeats puzzle) (size puzzle)
    pM = Predicate $ hasMandatory (mandatory puzzle)
    pL = Predicate $ checkLettersPool (repeats puzzle) (letters puzzle)

-- | Check word length based on whether repeats are allowed.
checkLength :: Bool -- ^ allow repeats?
            -> Int  -- ^ minimum word size
            -> BS.ByteString -- ^ word to check
            -> Bool -- ^ true if word length is valid
checkLength True  s = (>= s) . BS.length
checkLength False s = inRange (s, 9) . BS.length

-- | Does word contain the mandatory letter?
hasMandatory :: Char -> BS.ByteString -> Bool
hasMandatory = BS.elem

-- | Bit representing a lowercase letter: bit 0 for @a@ to bit 25 for @z@.
-- Any other character has no bit (zero), so it is never in a pool.
--
-- >>> bitOf 'a'
-- 1
--
-- >>> bitOf 'c'
-- 4
--
-- >>> bitOf 'A'
-- 0
bitOf :: Char -> Int
bitOf c
  | i >= 0 && i < 26 = 1 `unsafeShiftL` i
  | otherwise        = 0
  where
    i = ord c - ord 'a'

-- | Bitmask of a letter pool: the union of 'bitOf' for each letter.
--
-- Compute this once per puzzle (see 'poolMask'), then test each dictionary
-- character with a single bitwise AND instead of scanning the letters.
--
-- >>> maskOf "abc"
-- 7
maskOf :: String -> Int
maskOf = foldl' (\m c -> m .|. bitOf c) 0

-- | Check if a word matches the letter pool based on whether repeats
-- are allowed.
--
-- Assumes that the letter pool is valid (see 'isLetters').
checkLettersPool :: Bool -- ^ allow repeats?
                  -> String -- ^ valid letters in letter pool
                  -> BS.ByteString -- ^ word to check
                  -> Bool -- ^ true if word matches letter pool
checkLettersPool True  = spellingBee
checkLettersPool False = nineLetters

-- | Check if a word contains only characters from a letters list, each used
-- at most once.
--
-- Uses an integer bitmask of the allowed letter pool (see 'maskOf') and a
-- strict left fold over the input word with accumulator @(seen, ok)@:
--
-- * @seen@ is a bitset of already seen letters.
--
-- * @ok@ tracks whether all checks have passed so far.
--
-- For each character, the fold:
--
-- * fails if the character bit is not in the pool mask,
--
-- * fails if the character bit is already set (repeated letter),
--
-- * otherwise sets the bit and continues.
--
-- Evaluates 'maskOf' once when partially applied.
--
-- Assumes that the letter pool is valid (see 'isLetters').
nineLetters ::
     String        -- ^ valid letters
  -> BS.ByteString -- ^ dictionary word to check
  -> Bool          -- ^ true if dictionary word matches letters
nineLetters ls =
  let !pool = maskOf ls
  in snd . BS.foldl' (step pool) (0 :: Int, True)
  where
    step pool (seen, ok) c
      | not ok          = (seen, False) -- already failed, skip checks
      | b .&. pool == 0 = (seen, False) -- character not in letter pool
      | seen .&. b /= 0 = (seen, False) -- character already seen (repeated)
      | otherwise       = (seen .|. b, True) -- continue with bit set
      where
        b = bitOf c

-- | Check if a word contains only characters from a letters list.
-- Repeating characters are allowed.
--
-- Uses an integer bitmask of the allowed letter pool (see 'maskOf') to test
-- each character in constant time. Evaluates 'maskOf' once when partially
-- applied.
--
-- Assumes that the letter pool is valid (see 'isLetters').
spellingBee ::
     String        -- ^ valid letters
  -> BS.ByteString -- ^ dictionary word to check
  -> Bool          -- ^ true if dictionary word matches letters
spellingBee ls =
  let !pool = maskOf ls
  in BS.all (\c -> bitOf c .&. pool /= 0)
