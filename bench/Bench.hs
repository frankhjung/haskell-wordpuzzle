module Main(main) where

import qualified Data.ByteString.Char8 as BS
import           Criterion.Main        (bench, bgroup, defaultMain, nf)
import           WordPuzzle            (nineLetters, spellingBee)

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "nineLetters" $ nf (nineLetters "cadevrsoi") (BS.pack "aardvarks")
    , bench "spellingBee" $ nf (spellingBee "mitncao") (BS.pack "atomic")
    ]
  ]
