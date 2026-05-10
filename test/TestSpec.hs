{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Data.Ix         (inRange)
import           Data.Validation (Validation (..))
import           Test.Hspec      (context, describe, hspec, it, shouldBe)
import           WordPuzzle      (ValidationError (..), nineLetters,
                                  spellingBee, validateLetters, validateSize)

main :: IO ()
main = hspec $ do

  describe "validateSize" $ do
    context "too small" $
      it "returns Failure" $
        validateSize 0 `shouldBe` Failure [InvalidSize (4,9) 0]
    context "just below minimum" $
      it "returns Failure" $
        validateSize 3 `shouldBe` Failure [InvalidSize (4,9) 3]
    context "too large" $
      it "returns Failure" $
        validateSize 10 `shouldBe` Failure [InvalidSize (4,9) 10]
    context "size in range" $
      it "returns Success" $
        validateSize 4 `shouldBe` Success 4

  describe "validateLetters" $ do
    context "fewer than 4 letters" $
      it "returns Failure" $
        validateLetters "abc" `shouldBe` Failure [InvalidLetters "abc"]
    context "4 lowercase letters (lower bound)" $
      it "returns Success" $
        validateLetters "abcd" `shouldBe` Success "abcd"
    context "mid-range lowercase letters" $
      it "returns Success" $
        validateLetters "abcdefg" `shouldBe` Success "abcdefg"
    context "9 lowercase letters (upper bound)" $
      it "returns Success" $
        validateLetters "abcdefghi" `shouldBe` Success "abcdefghi"
    context "mixed case letters" $
      it "returns Failure" $
        validateLetters "abcdeFghi" `shouldBe` Failure [InvalidLetters "abcdeFghi"]
    context "duplicate characters" $
      it "returns Failure" $
        validateLetters "abca" `shouldBe` Failure [InvalidLetters "abca"]
    context "too many letters" $
      it "returns Failure" $
        validateLetters "abcdefghij" `shouldBe` Failure [InvalidLetters "abcdefghij"]

  describe "nineLetters" $ do
    -- use a valid pool (unique letters, length between 4 and 9)
    context "when word contains valid characters" $
      it "returns true" $
        nineLetters "abcdef" "abcdef" `shouldBe` True
    context "when word contains a valid subset of characters" $
      it "returns true" $
        nineLetters "abcdef" "fedc" `shouldBe` True
    context "when word does not contain valid characters" $
      it "returns false" $
        nineLetters "abcdef" "zapd" `shouldBe` False
    context "when word does not contain valid character frequency" $
      it "returns false" $
        nineLetters "abcdef" "aabdefc" `shouldBe` False
    context "when the letter pool is invalid" $
      it "returns false" $
        nineLetters "abca" "abc" `shouldBe` False
    context "when the letter pool is too short" $
      it "returns false" $
        nineLetters "abc" "abc" `shouldBe` False
    context "when the letter pool has uppercase letters" $
      it "returns false" $
        nineLetters "Abcd" "abc" `shouldBe` False

  describe "spellingBee" $ do
    context "when word contains valid characters" $
      it "returns true" $
        spellingBee "barfo" "barfoo" `shouldBe` True
    context "when word does not contain valid characters" $
      it "returns false" $
        spellingBee "barfo" "bartez" `shouldBe` False
    context "when the letter pool is invalid" $
      it "returns false" $
        spellingBee "abca" "abc" `shouldBe` False
    context "when the letter pool is too short" $
      it "returns false" $
        spellingBee "abc" "abc" `shouldBe` False
    context "when the letter pool has uppercase letters" $
      it "returns false" $
        spellingBee "Abcd" "abc" `shouldBe` False

  describe "solver length predicate" $ do
    let pS :: Bool -> Int -> String -> Bool
        pS repeats size = if repeats
                          then (>= size) . length
                          else inRange (size, 9) . length
    context "repeats allowed" $
      it "permits words longer than the letter pool" $
        pS True 4 "aaaaaaaa" `shouldBe` True
    context "repeats forbidden" $
      it "rejects words longer than 9 characters" $
        pS False 4 "abcdefghij" `shouldBe` False
