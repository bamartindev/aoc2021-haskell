module Solutions.DayEight
  ( d8p1,
    d8p2,
  )
where

import Data.List (findIndex, intersect, sort, subsequences, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

d8p1 :: [String] -> IO ()
d8p1 input = do
  let entries = map parseEntry input
  print $ sum $ map countEasy entries

d8p2 :: [String] -> IO ()
d8p2 input = do
  let entries = map parseEntry input
  print $ sum $ map calc entries

--------------
-- Start P1
--------------
type Pattern = String

data Entry = Entry
  { signalPatterns :: [Pattern],
    output :: [String]
  }
  deriving (Show)

parseEntry :: String -> Entry
parseEntry e = Entry patterns' output'
  where
    [patterns, output] = splitOn " | " e
    patterns' = splitOn " " patterns
    output' = splitOn " " output

countEasy :: Entry -> Int
countEasy (Entry _ output) = length $ filter (\x -> length x `elem` [2, 3, 4, 7]) output

--------------
-- Start P2
--------------
calc :: Entry -> Int
calc (Entry p o) = foldl addDigit 0 $ map (decode' mapped) o
  where
    addDigit n d = 10 * n + d
    mapped = map (getPattern p) [0 .. 9]

decode' :: [Pattern] -> Pattern -> Int
decode' ps p = case findIndex f ps of
  Just i -> i
  Nothing -> error "Couldn't find the pattern in the list of patterns"
  where
    f a = sort p == sort a

getPattern :: [Pattern] -> Int -> Pattern
getPattern p 0 = head $ filter (\x -> length x == 6 && x `notElem` [getPattern p 6, getPattern p 9]) p
getPattern p 1 = getSimple p 2
getPattern p 2 = head $ filter (\x -> length x == 5 && x `notElem` [getPattern p 3, getPattern p 5]) p
getPattern p 3 = head $ filter (matchPattern (getPattern p 1) 5) p
getPattern p 4 = getSimple p 4
getPattern p 5 = head $ filter (matchPattern (diff4 p) 5) p
getPattern p 6 = head $ filter (\x -> x /= getPattern p 9 && matchPattern (diff4 p) 6 x) p
getPattern p 7 = getSimple p 3
getPattern p 8 = getSimple p 7
getPattern p 9 = head $ filter (matchPattern (getPattern p 4) 6) p
getPattern _ _ = error "Cannot find pattern"

matchPattern :: Pattern -> Int -> (Pattern -> Bool)
matchPattern val len x = length x == len && (val `intersect` x == val)

getSimple :: [Pattern] -> Int -> Pattern
getSimple p len = head $ filter (\x -> length x == len) p

diff4 :: [Pattern] -> Pattern
diff4 p = getPattern p 4 \\ getPattern p 1
