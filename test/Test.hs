module Main(main) where

import           Test.Hspec (context, describe, hspec, it, shouldBe)
import           WordPuzzle (isPlural, isValid, remove)

main :: IO ()
main = hspec $ do

  describe "remove" $ do
    context "when character is in list" $
      it "returns list less that character" $
        remove 'c' "cde" `shouldBe` "de"
    context "when character is in list twice" $
      it "returns list less one instance of that character" $
        remove 'c' "cce" `shouldBe` "ce"
    context "when character is not in list" $
      it "returns original list" $
        remove 'c' "def" `shouldBe` "def"

  describe "isValid" $ do
    context "when word containing characters" $
      it "returns true" $
        isValid "foobar" "barfoo" `shouldBe` True
    context "when word containing a valid subset of characters" $
      it "returns true" $
        isValid "foobar" "rof" `shouldBe` True
    context "when word does not contain valid characters" $
      it "returns false" $
        isValid "foobar" "bartez" `shouldBe` False
    context "when word does not contain valid character frequency" $
      it "returns false" $
        isValid "foobar" "baarof" `shouldBe` False

  describe "isPlural" $ do
    context "when word ends in 'ss'" $
      it "returns false" $
        isPlural "foobass" `shouldBe` False
    context "when word does not end in 's'" $
      it "returns false" $
        isPlural "foobar" `shouldBe` False
    context "when word ends in 's'" $
      it "returns true" $
        isPlural "foobars" `shouldBe` True
