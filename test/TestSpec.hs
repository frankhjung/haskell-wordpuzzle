{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import qualified Data.ByteString.Char8 as BS
import           Data.Ix               (inRange)
import           Data.Validation       (Validation (..))
import qualified System.IO.Streams     as Streams
import           Test.Hspec            (context, describe, hspec, it, shouldBe)
import           WordPuzzle            (ValidationError (..), mkWordPuzzle,
                                        nineLetters, solver, spellingBee,
                                        toEither, validateLetters, validateSize)

-- | Extract a 'Success' value or fail the test.
unsafeMk :: Show e => Validation [e] a -> a
unsafeMk (Success a) = a
unsafeMk (Failure e) =
  error $ "test setup: " ++ show e

main :: IO ()
main = hspec $ do

  describe "validateSize" $ do
    context "too small" $
      it "returns Failure" $
        validateSize 0
          `shouldBe` Failure [InvalidSize (4,9) 0]
    context "just below minimum" $
      it "returns Failure" $
        validateSize 3
          `shouldBe` Failure [InvalidSize (4,9) 3]
    context "too large" $
      it "returns Failure" $
        validateSize 10
          `shouldBe` Failure [InvalidSize (4,9) 10]
    context "size in range" $
      it "returns Success" $
        validateSize 4 `shouldBe` Success 4

  describe "validateLetters" $ do
    context "fewer than 4 letters" $
      it "returns Failure" $
        validateLetters "abc"
          `shouldBe` Failure [InvalidLetters "abc"]
    context "4 lowercase letters (lower bound)" $
      it "returns Success" $
        validateLetters "abcd"
          `shouldBe` Success "abcd"
    context "mid-range lowercase letters" $
      it "returns Success" $
        validateLetters "abcdefg"
          `shouldBe` Success "abcdefg"
    context "9 lowercase letters (upper bound)" $
      it "returns Success" $
        validateLetters "abcdefghi"
          `shouldBe` Success "abcdefghi"
    context "mixed case letters" $
      it "returns Failure" $
        validateLetters "abcdeFghi"
          `shouldBe`
            Failure [InvalidLetters "abcdeFghi"]
    context "duplicate characters" $
      it "returns Failure" $
        validateLetters "abca"
          `shouldBe` Failure [InvalidLetters "abca"]
    context "too many letters" $
      it "returns Failure" $
        validateLetters "abcdefghij"
          `shouldBe`
            Failure [InvalidLetters "abcdefghij"]

  describe "mkWordPuzzle" $ do
    context "valid inputs" $
      it "returns Success" $
        toEither (mkWordPuzzle False 4 "abcd" "dict")
          `shouldBe`
            Right (unsafeMk $
              mkWordPuzzle False 4 "abcd" "dict")
    context "invalid size" $
      it "returns Failure with InvalidSize" $
        toEither (mkWordPuzzle False 2 "abcd" "dict")
          `shouldBe`
            Left [InvalidSize (4,9) 2]
    context "invalid letters" $
      it "returns Failure with InvalidLetters" $
        toEither
          (mkWordPuzzle False 4 "AB" "dict")
            `shouldBe`
              Left [InvalidLetters "AB"]
    context "empty letters" $
      it "returns Failure with InvalidLetters" $
        toEither (mkWordPuzzle False 4 "" "dict")
          `shouldBe`
            Left [InvalidLetters "empty letters"]
    context "multiple errors accumulated" $
      it "collects both size and letter errors" $
        toEither (mkWordPuzzle False 2 "AB" "dict")
          `shouldBe`
            Left [ InvalidSize (4,9) 2
                 , InvalidLetters "AB"
                 ]

  describe "nineLetters" $ do
    -- use a valid pool (unique letters, 4-9)
    context "word contains valid characters" $
      it "returns true" $
        nineLetters "abcdef" "abcdef"
          `shouldBe` True
    context "word contains valid subset" $
      it "returns true" $
        nineLetters "abcdef" "fedc"
          `shouldBe` True
    context "word has invalid characters" $
      it "returns false" $
        nineLetters "abcdef" "zapd"
          `shouldBe` False
    context "word has invalid frequency" $
      it "returns false" $
        nineLetters "abcdef" "abadefc"
          `shouldBe` False

  describe "spellingBee" $ do
    context "word contains valid characters" $
      it "returns true" $
        spellingBee "barfo" "barfoo"
          `shouldBe` True
    context "word has invalid characters" $
      it "returns false" $
        spellingBee "barfo" "bartez"
          `shouldBe` False

  describe "solver length predicate" $ do
    let pS :: Bool -> Int -> BS.ByteString -> Bool
        pS r s = if r
                 then (>= s) . BS.length
                 else inRange (s, 9) . BS.length
    context "repeats allowed" $
      it "permits words longer than the pool" $
        pS True 4 "aaaaaaaa" `shouldBe` True
    context "repeats forbidden" $
      it "rejects words longer than 9" $
        pS False 4 "abcdefghij" `shouldBe` False

  describe "solver" $ do
    it "filters words in nine letters mode" $ do
      let puzzle = unsafeMk $
            mkWordPuzzle False 4 "abcd" "dictionary"
      is <- Streams.fromList
        ["abcd", "bcde", "aabc", "aaaa", "abcde"]
      os <- solver puzzle is
      res <- Streams.toList os
      res `shouldBe` ["abcd"]

    it "filters words in spelling bee mode" $ do
      let puzzle = unsafeMk $
            mkWordPuzzle True 4 "abcd" "dictionary"
      is <- Streams.fromList
        ["abcd", "bcde", "aabc", "aaaa", "abcde"]
      os <- solver puzzle is
      res <- Streams.toList os
      res `shouldBe` ["abcd", "aabc", "aaaa"]
