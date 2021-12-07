module Solutions.DaySeven
  ( d7p1,
    d7p2,
  )
where

import Data.List (sort)

-- Pretty simple brute force.  There is probably a mathy way to do this to find the most central point to all points
-- and just do the distance compute once instead of against the whole range :)

d7p1 :: [Int] -> IO ()
d7p1 input = print $ minimum $ map (totalDistance input) [minimum input .. maximum input]

d7p2 :: [Int] -> IO ()
d7p2 input = print $ minimum $ map (totalDistance' input) [minimum input .. maximum input]

totalDistance :: [Int] -> Int -> Int
totalDistance p i = sum $ map (\x -> curry distance x i) p

totalDistance' :: [Int] -> Int -> Int
totalDistance' p i = sum $ map (\x -> curry distance' x i) p

distance :: (Int, Int) -> Int
distance (x1, x2) = abs (x1 - x2)

distance' :: (Int, Int) -> Int
distance' (x1, x2) = sum [1 .. abs (x1 - x2)]
