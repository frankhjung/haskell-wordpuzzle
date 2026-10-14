module Main(main) where

import           Criterion.Main        (bench, bgroup, defaultMain, env, nf)
import qualified Data.ByteString.Char8 as BS
import           WordPuzzle            (nineLetters, spellingBee)

-- | Number of words in the dictionary accepted by a predicate.
countMatches :: (BS.ByteString -> Bool) -> [BS.ByteString] -> Int
countMatches p = length . filter p

main :: IO ()
main = defaultMain
  [
    bgroup "WordPuzzle"
    [
      bench "nineLetters" $ nf (nineLetters "cadevrsoi") (BS.pack "aardvarks")
    , bench "spellingBee" $ nf (spellingBee "mitncao") (BS.pack "atomic")
    ]
  , env (BS.lines <$> BS.readFile "dictionary") $ \ws ->
    bgroup "Dictionary"
    [
      bench "nineLetters" $ nf (countMatches (nineLetters "cadevrsoi")) ws
    , bench "spellingBee" $ nf (countMatches (spellingBee "cadevrsoi")) ws
    ]
  ]
