module Solutions.DaySix
  ( d6p1,
    d6p2,
  )
where

import Data.Foldable (find)
import qualified Data.Map.Strict as M

d6p1 :: [Int] -> IO ()
d6p1 input = do
  print $ count $ simulate 80 (gatherFish input)

d6p2 :: [Int] -> IO ()
d6p2 input = do
  print $ count $ simulate 256 (gatherFish input)

gatherFish :: [Int] -> [(Int, Int)]
gatherFish input = fishCounts $ zip input (repeat 1)

count :: [(Int, Int)] -> Int
count f = sum $ map snd f

fishCounts :: (Ord k, Num a) => [(k, a)] -> [(k, a)]
fishCounts fish = M.toList $ M.fromListWith (+) fish

simulate :: Int -> [(Int, Int)] -> [(Int, Int)]
simulate 0 fish = fish
simulate d fish = simulate (d - 1) (fishCounts next)
  where
    next = case new fish of
      Just (_, c) -> (8, c) : map tick fish
      Nothing -> map tick fish

new :: [(Int, Int)] -> Maybe (Int, Int)
new = find (\f -> fst f == 0)

tick :: (Int, Int) -> (Int, Int)
tick (0, c) = (6, c)
tick (n, c) = (n - 1, c)
