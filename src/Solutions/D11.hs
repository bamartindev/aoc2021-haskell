module Solutions.D11 (d11p1, d11p2) where

import Data.Char (digitToInt)
import Data.List (findIndex, foldl', (\\))

d11p1 :: [String] -> IO ()
d11p1 input = do
  let grid = mkGrid input
  print $ simulate grid 100

d11p2 :: [String] -> IO ()
d11p2 input = do
  let grid = mkGrid input
  print $ findFirstSync grid 1

type Point = (Int, Int)

type Grid = [(Point, Int)]

mkGrid :: [String] -> Grid
mkGrid xs = [((x, y), digitToInt n) | (y, row) <- zip [0 ..] xs, (x, n) <- zip [0 ..] row]

adjacent :: Point -> [Point]
adjacent (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y)]

simulate :: Grid -> Int -> Int
simulate g 0 = 0
simulate g i = n + simulate g' (i - 1)
  where
    (n, g') = simFlash (inc g) []

findFirstSync :: Grid -> Int -> Int
findFirstSync g i = if allFlash then i else findFirstSync g' (i + 1)
  where
    allFlash = all (\(_, n) -> n == 0) g'
    (n, g') = simFlash (inc g) []

simFlash :: Grid -> [Point] -> (Int, Grid)
simFlash g seen =
  if not (null flashes)
    then simFlash next (flashes ++ seen)
    else (length seen, resetFlashed next)
  where
    flashes = possibleFlashes g \\ seen
    next = foldl' inc' g (flashIndicies flashes)

-- Increment all points
inc :: Grid -> Grid
inc = map (\(p, n) -> (p, n + 1))

-- Increment a specific point.
inc' :: Grid -> Point -> Grid
inc' g (x, y) = let (f, (_, n) : b) = splitAt index g in f ++ ((x, y), n + 1) : b
  where
    index = case findIndex (\((x', y'), _) -> x == x' && y == y') g of
      Just i -> i
      Nothing -> error "Failed to get index in inc'"

-- Functions to help find which point is flashing
-- and adjacent points to inc
possibleFlashes :: Grid -> [Point]
possibleFlashes g = map fst $ filter (\(_, n) -> n > 9) g

flashIndicies :: [Point] -> [Point]
flashIndicies flashes = validIndicies $ concatMap adjacent flashes

validIndicies :: [Point] -> [Point]
validIndicies xs = [(x, y) | (x, y) <- xs, x >= 0, y >= 0, x < 10, y < 10]

-- Reset all points that flashed
resetFlashed :: Grid -> Grid
resetFlashed = map (\(p, n) -> if n > 9 then (p, 0) else (p, n))
