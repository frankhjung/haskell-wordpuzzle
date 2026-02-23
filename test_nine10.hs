import Data.List (delete, nub)
import Data.Ix (inRange)
import Data.Char (isLower)

isLetters :: String -> Bool
isLetters ls =
  inRange (4,9) n && all isLower ls && length (nub ls) == n
  where
    n = length ls

nineLetters :: String -> String -> Bool
nineLetters ls ys = isLetters ls && go ls ys
  where
    go _  []     = True
    go [] _      = False
    go (x:xs) ys = go xs (delete x ys)

main = do
  print $ nineLetters "abcdef" "cab"
