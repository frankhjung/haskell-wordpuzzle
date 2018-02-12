{-# LANGUAGE UnicodeSyntax #-}

import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "greet" $
    it "greet returns IO ()'" $
      greet "Hello World" `shouldReturn` ()

