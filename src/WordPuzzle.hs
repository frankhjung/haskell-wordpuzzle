{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

{-|
  Module      : WordPuzzle
  Description : Word Puzzle supporting functions.
  Copyright   : © Frank Jung, 2017-2019
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
                  , removeLetter
                  , solve
                  , ValidationError(..)
                  ) where

import           Data.ByteString.Char8 (ByteString, cons, elem, empty, length,
                                        uncons)
import           Data.Char             (isLower)
import           Data.Ix               (inRange)
import           Prelude               hiding (elem, length)
import qualified Prelude               (elem, length)
import           System.IO             (IOMode (ReadMode), withFile)
import qualified System.IO.Streams     as Streams (connect, filter,
                                                   handleToInputStream, lines,
                                                   stdout, unlines)

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
isMandatory = Prelude.elem

-- | Check that mandatory letter.
-- TODO Can we use 'isMandatory' here? It will require an extra parameter,
-- which is not available by the parser.
checkMandatory :: String              -- ^ mandatory character to check
                -> Either String Char -- ^ valid mandatory letter
checkMandatory [] = Left (show (UnexpectedValue ""))
checkMandatory ms@(m:_)
  | 1 /= Prelude.length ms  = Left (show (UnexpectedValue ms))
  | not (isLower m)         = Left (show (InvalidMandatory m))
  | otherwise               = Right m

-- | Are letters valid?
isLetters :: String -> Bool
isLetters ls = 9 == Prelude.length ls && all isLower ls

-- | Check that letters are lowercase alphabetic characters.
checkLetters :: String                -- ^ characters to check
              -> Either String String -- ^ valid lowercase letters
checkLetters ls = if isLetters ls
                  then Right ls
                  else Left (show (InvalidLetters ls))

-- | Smart constructor for WordPuzzle.
-- TODO re-write using accumulative validation
makeWordPuzzle :: Int -> Char -> String -> FilePath -> Either ValidationError WordPuzzle
makeWordPuzzle s m ls d
  | not (isSize s)          = Left (InvalidSize s)
  | not (isLetters ls)      = Left (InvalidLetters ls)
  | not (isMandatory m ls)  = Left (InvalidMandatory m)
  | otherwise               = Right (WordPuzzle s m ls d)

-- | Solve puzzle.
-- TODO - return list rather than IO ()
--
-- Print words to stdout where:
--
-- 1. must be greater than the minimum word length
-- 2. must be no more than 9 characters long
-- 3. must contain mandatory character
-- 4. must contain only valid characters
-- 5. must not exceed valid character frequency
solve :: WordPuzzle -> IO ()
solve puzzle =
  withFile (dictionary puzzle) ReadMode $ \handle -> do
    inWords <- Streams.handleToInputStream handle >>=
                Streams.lines >>=
                Streams.filter (inRange (size puzzle, 9) . length) >>=
                Streams.filter (elem (mandatory puzzle)) >>=
                Streams.filter (isWord (letters puzzle))
    outWords <- Streams.unlines Streams.stdout
    Streams.connect inWords outWords

-- | Pattern for an empty ByteString.
pattern Empty :: ByteString
pattern Empty <- (uncons -> Nothing)

-- | Check if a word contains only characters from a list.
--
-- * If all valid characters are removed from the word, and there are still
-- characters left over, then the word is not valid.
--
-- * If all valid characters are removed from the word, and the word is
-- empty, then the word is valid.
isWord :: String      -- ^ valid letters
        -> ByteString -- ^ dictionary word to check
        -> Bool       -- ^ true if dictionary word matches letters
isWord _  Empty  = True
isWord [] _      = False
isWord (x:xs) ys = if x `elem` ys
                   then isWord xs (removeLetter x ys)
                   else isWord xs ys

-- | Remove first occurrence of a character from a word.
--
-- Used by 'isWord' to remove an element that is guaranteed to be present.
removeLetter :: Char         -- ^ character to remove
              -> ByteString  -- ^ string to remove character from
              -> ByteString  -- ^ result after character removed
removeLetter _ Empty = empty
removeLetter x ys = if x == h then ts else h `cons` removeLetter x ts
  where Just (h, ts) = uncons ys
