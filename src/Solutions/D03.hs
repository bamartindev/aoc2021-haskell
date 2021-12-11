module Solutions.D03
  ( d3p1,
    d3p2,
  )
where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldr'))
import Data.List (foldl', sort, transpose)

-- ----------------------
-- EXPORTED FUNCTIONS
-- ----------------------
d3p1 :: [String] -> IO ()
d3p1 input = do
  let bits = transpose input
  let gamma = gammaRate bits
  let epsilon = epsilonRate bits

  print $ gamma * epsilon

d3p2 :: [String] -> IO ()
d3p2 input = do
  let oxygenRating = oxygenGeneratorRating 0 input
  let c02Rating = c02ScrubberRating 0 input

  print $ oxygenRating * c02Rating

-- ----------------------
-- PART 1 MAIN FUNCTIONS
-- ----------------------
gammaRate :: [String] -> Int
gammaRate bits = toDec $ map mostCommon bits

epsilonRate :: [String] -> Int
epsilonRate bits = toDec $ map leastCommon bits

-- ----------------------
-- PART 2 MAIN FUNCTIONS
-- ----------------------
oxygenGeneratorRating :: Int -> [String] -> Int
oxygenGeneratorRating _ [] = 0
oxygenGeneratorRating _ [x] = toDec x
oxygenGeneratorRating i xs
  | isTie bits = oxygenGeneratorRating (i + 1) $ filter (\b -> '1' == b !! i) xs
  | otherwise = oxygenGeneratorRating (i + 1) $ filter (\b -> mc == b !! i) xs
  where
    bits = map (!! i) xs
    mc = mostCommon bits

c02ScrubberRating :: Int -> [String] -> Int
c02ScrubberRating _ [] = 0
c02ScrubberRating _ [x] = toDec x
c02ScrubberRating i xs
  | isTie bits = c02ScrubberRating (i + 1) $ filter (\b -> '0' == b !! i) xs
  | otherwise = c02ScrubberRating (i + 1) $ filter (\b -> lc == b !! i) xs
  where
    bits = map (!! i) xs
    lc = leastCommon bits

-- ----------------------
-- HELPER FUNCTIONS
-- ----------------------

mostCommon :: Ord a => [a] -> a
mostCommon = snd . last . result

leastCommon :: Ord a => [a] -> a
leastCommon = snd . head . result

isTie :: Ord a => [a] -> Bool
isTie xs = mostCommon xs == leastCommon xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(count x xs, x) | x <- rmdups xs]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
