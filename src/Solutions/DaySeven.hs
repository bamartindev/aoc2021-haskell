module Solutions.DaySeven
  ( d7p1,
    d7p2,
  )
where

import Data.List (nub)

d7p1 :: [Int] -> IO ()
d7p1 input = print $ minimum $ map (totalDistance input) $ nub input

d7p2 :: [Int] -> IO ()
d7p2 input = print $ minimum $ map (totalDistance' input) [minimum input .. maximum input]

totalDistance :: [Int] -> Int -> Int
totalDistance p i = sum $ map (\x -> curry distance x i) p

totalDistance' :: [Int] -> Int -> Int
totalDistance' p i = sum $ map (\x -> curry distance' x i) p

distance :: (Int, Int) -> Int
distance (x1, x2) = abs (x1 - x2)

distance' :: (Int, Int) -> Int
distance' (x1, x2) = (n * (n + 1)) `div` 2
  where
    n = abs (x1 - x2)
