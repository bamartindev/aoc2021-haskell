module Solutions.Day11 where --(d11p1, d11p2,) where

import Data.Char (digitToInt)
import Data.List (findIndex, foldl')

d11p1 :: [String] -> IO ()
d11p1 input = do
  let grid = mkGrid input
  print $ simulate grid 100

d11p2 :: [String] -> IO ()
d11p2 input = do
  let grid = mkGrid input
  print $ findFirstSync grid 1

findFirstSync :: Grid -> Int -> Int
findFirstSync g i = if allFlash then i else findFirstSync f (i + 1)
  where
    allFlash = all (\(_, n) -> n == 0) f
    next = inc g
    (n, f) = flash next []

type Grid = [((Int, Int), Int)]

mkGrid :: [String] -> Grid
mkGrid xs = [((x, y), digitToInt n) | (y, row) <- zip [0 ..] xs, (x, n) <- zip [0 ..] row]

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y)]

simulate :: Grid -> Int -> Int
simulate g 0 = 0
simulate g i = n + simulate f (i - 1)
  where
    next = inc g
    (n, f) = flash next []

findFlashes :: Grid -> Grid
findFlashes = filter (\(_, n) -> n > 9)

flashIndicies :: Grid -> [(Int, Int)]
flashIndicies flashes = validIndicies $ concatMap (adjacent . fst) flashes

validIndicies :: [(Int, Int)] -> [(Int, Int)]
validIndicies xs = [(x, y) | (x, y) <- xs, x >= 0, y >= 0, x < 10, y < 10]

inc :: Grid -> Grid
inc = map (\(p, n) -> (p, n + 1))

resetFlashed :: Grid -> Grid
resetFlashed = map (\(p, n) -> if n > 9 then (p, 0) else (p, n))

inc' :: Grid -> (Int, Int) -> Grid
inc' g (x, y) = let (f, (_, n) : b) = splitAt index g in f ++ ((x, y), n + 1) : b
  where
    index = case findIndex (\((x', y'), _) -> x == x' && y == y') g of
      Just i -> i
      Nothing -> error "Failed to get index in inc'"

-- TODO (Brett) Rewrite this nonsense!
flash :: Grid -> [(Int, Int)] -> (Int, Grid)
flash g seen = if numFlashes > 0 then let (numFlashes', g') = flash next seen' in (numFlashes', g') else (length seen, resetFlashed next)
  where
    g' = inc g
    numFlashes = length flashes
    flashes = filter (\(i, _) -> i `notElem` seen) (findFlashes g)
    indicies = flashIndicies flashes
    next = foldl' inc' g indicies
    seen' = map fst flashes ++ seen

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"
