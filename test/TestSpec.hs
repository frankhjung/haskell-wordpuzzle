{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Test.Hspec (context, describe, hspec, it, shouldBe)
import           WordPuzzle (ValidationError (..), checkLetters, checkMandatory,
                             checkSize, isWord, removeLetter)

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

  describe "checkMandatory" $ do
    context "valid mandatory" $
      it "1 lowercase mandatory" $
        checkMandatory "a" `shouldBe` Right 'a'
    context "uppercase mandatory" $
      it "returns Left" $
        checkMandatory "A" `shouldBe` Left (show (InvalidMandatory 'A'))
    context "empty mandatory" $
      it "returns Left" $
        checkMandatory "" `shouldBe` Left (show (UnexpectedValue ""))
    context "more than 1 mandatory" $
      it "returns Left" $
        checkMandatory "ab" `shouldBe` Left (show (UnexpectedValue "ab"))

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

  describe "removeLetter" $ do
    context "when character is in list" $
      it "returns list less that character" $
        removeLetter 'c' "cde" `shouldBe` "de"
    context "when character is in list twice" $
      it "returns list less one instance of that character" $
        removeLetter 'c' "cce" `shouldBe` "ce"
    context "when character is not in list" $
      it "returns original list" $
        removeLetter 'c' "def" `shouldBe` "def"

  describe "isWord" $ do
    context "when word contains valid characters" $
      it "returns true" $
        isWord "foobar" "barfoo" `shouldBe` True
    context "when word contains a valid subset of characters" $
      it "returns true" $
        isWord "foobar" "rof" `shouldBe` True
    context "when word does not contain valid characters" $
      it "returns false" $
        isWord "foobar" "bartez" `shouldBe` False
    context "when word does not contain valid character frequency" $
      it "returns false" $
        isWord "foobar" "baarof" `shouldBe` False

