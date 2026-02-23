import Data.List (delete, nub)
import Data.Ix (inRange)
import Data.Char (isLower)

isLetters :: String -> Bool
isLetters ls =
  inRange (4,9) n && all isLower ls && length (nub ls) == n
  where
    n = length ls

spellingBee :: String -> String -> Bool
spellingBee ls ys = isLetters ls && go ls ys
  where
    go _ []     = True
    go ls (y:ys)
      | y `elem` ls = go ls ys
      | otherwise   = False

main = do
  print $ spellingBee "abcdef" "cab"
