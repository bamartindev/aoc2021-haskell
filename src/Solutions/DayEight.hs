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
  print $ sum $ map getOutput entries

--------------
-- Start P1
--------------
type Pattern = String

type Entry = ([Pattern], [Pattern])

parseEntry :: String -> Entry
parseEntry e = (patterns', output')
  where
    [patterns, output] = splitOn " | " e
    patterns' = splitOn " " patterns
    output' = splitOn " " output

countEasy :: Entry -> Int
countEasy (_, output) = length $ filter (\x -> length x `elem` [2, 3, 4, 7]) output

--------------
-- Part 2
-- --------------
getOutput :: Entry -> Int
getOutput (patterns, output) = foldl addDigit 0 $ map (decode patterns) output
  where
    addDigit n d = 10 * n + d

decode :: [Pattern] -> Pattern -> Int
decode ps p = case length p of
  2 -> 1
  3 -> 7
  4 -> 4
  5 -> fiveSegment ps p
  6 -> sixSegment ps p
  7 -> 8
  n -> error $ "Cannot decode length " ++ show n
  where
    fiveSegment ps p
      | contains (getKnown 1 ps) p = 3
      | contains (diff4' ps) p = 5
      | otherwise = 2
    sixSegment ps p
      | contains (getKnown 4 ps) p = 9
      | contains (diff4' ps) p = 6
      | otherwise = 0

contains :: (Eq a) => [a] -> [a] -> Bool
contains a b = a `intersect` b == a

getKnown :: Int -> [Pattern] -> Pattern
getKnown 1 ps = head $ filterByLen 2 ps
getKnown 4 ps = head $ filterByLen 4 ps
getKnown 7 ps = head $ filterByLen 3 ps
getKnown 8 ps = head $ filterByLen 7 ps
getKnown n _ = error $ " Unknow pattern for number: " ++ show n

filterByLen :: Int -> [Pattern] -> [Pattern]
filterByLen len = filter (\x -> length x == len)

diff4' :: [Pattern] -> Pattern
diff4' ps = getKnown 4 ps \\ getKnown 1 ps
