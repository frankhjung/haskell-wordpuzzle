{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Test.Hspec (context, describe, hspec, it, shouldBe)
import           WordPuzzle (ValidationError (..), checkLetters, checkSize,
                             hasLetters)

main :: IO ()
main = hspec $ do

  describe "checkSize" $ do
    context "size outside range" $
      it "returns Left" $
        checkSize 0 `shouldBe` Left (show (InvalidSize 0))
    context "size outside range" $
      it "returns Left" $
        checkSize 10 `shouldBe` Left (show (InvalidSize 10))
    context "size in range" $
      it "returns Right" $
        checkSize 4 `shouldBe` Right 4

  describe "checkLetters" $ do
    context "too few letters" $
      it "returns Left" $
        checkLetters "abcdefgh" `shouldBe` Left (show (InvalidLetters "abcdefgh"))
    context "9 lowercase letters" $
      it "returns Right" $
        checkLetters "abcdefghi" `shouldBe` Right "abcdefghi"
    context "9 mixed case letters" $
      it "returns Left" $
        checkLetters "abcdeFghi" `shouldBe` Left (show (InvalidLetters "abcdeFghi"))
    context "too many letters" $
      it "returns Left" $
        checkLetters "abcdefghij" `shouldBe` Left (show (InvalidLetters "abcdefghij"))

  describe "hasLetters" $ do
    context "when word contains valid characters" $
      it "returns true" $
        hasLetters "foobar" "barfoo" `shouldBe` True
    context "when word contains a valid subset of characters" $
      it "returns true" $
        hasLetters "foobar" "rof" `shouldBe` True
    context "when word does not contain valid characters" $
      it "returns false" $
        hasLetters "foobar" "bartez" `shouldBe` False
    context "when word does not contain valid character frequency" $
      it "returns false" $
        hasLetters "foobar" "baarof" `shouldBe` False
