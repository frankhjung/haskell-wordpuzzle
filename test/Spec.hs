import           Test.Hspec (context, describe, hspec, it, shouldBe)
import           WordPuzzle (delete, filterWords, isInValid, isValid)

main :: IO ()
main = hspec $ do

  describe "delete" $ do
    context "when character is in list" $
      it "returns list less that character" $
        delete 'c' "cde" `shouldBe` "de"
    context "when character is in list twice" $
      it "returns list less one instance of that character" $
        delete 'c' "cce" `shouldBe` "ce"
    context "when character is not in list" $
      it "returns original list" $
        delete 'c' "def" `shouldBe` "def"

  describe "filterWords" $ do
    context "when word is too short" $
      it "returns false" $
        filterWords 3 'h' "hello" "he" `shouldBe` False
    context "when word does not contain mandatory character" $
      it "returns false" $
        filterWords 3 'h' "hello" "fello" `shouldBe` False
    context "when word does not contain a valid character" $
      it "returns false" $
        filterWords 3 'h' "hello" "hallo" `shouldBe` False
    context "when word contains characters exceeding expected frequency" $
      it "returns false" $
        filterWords 3 'h' "hello" "hollo" `shouldBe` False
    context "when word is valid" $
      it "returns true" $
        filterWords 4 'h' "hello" "hello" `shouldBe` True

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

  describe "isInValid" $ do
    context "when word containing characters" $
      it "returns false" $
        isInValid "foobar" "barfoo" `shouldBe` False
    context "when word containing a valid subset of characters" $
      it "returns false" $
        isInValid "foobar" "rof" `shouldBe` False
    context "when word does not contain valid characters" $
      it "returns true" $
        isInValid "foobar" "bartez" `shouldBe` True
    context "when word does not contain valid character frequency" $
      it "returns true" $
        isInValid "foobar" "baarof" `shouldBe` True

