module Solutions.DayNine
  ( d9p1,
    d9p2,
  )
where

import Data.Char (digitToInt)
import Data.List (nub, sortBy, (\\))

d9p1 :: [String] -> IO ()
d9p1 input = do
  let (width, height, grid) = mkGrid input
  let lowPoints = [x | (i, x) <- grid, isLowPoint x (map (getValAtIndex width grid) $ adjacentIndices width height grid i)]

  print $ sum lowPoints + length lowPoints

d9p2 :: [String] -> IO ()
d9p2 input = do
  let (width, height, grid) = mkGrid input
  let lowPoints = [(i, x) | (i, x) <- grid, isLowPoint x (map (getValAtIndex width grid) $ adjacentIndices width height grid i)]

  let sizes = sortBy (flip compare) $ map (basinSize width height grid . fst) lowPoints
  let result = product $ take 3 sizes

  print result

--------------
-- Start P1
--------------

type Width = Int

type Height = Int

type Grid = [(Int, Int)]

strToInts :: String -> [Int]
strToInts = map digitToInt

mkGrid :: [String] -> (Width, Height, Grid)
mkGrid input = do
  let grid = map strToInts input
  let flattened = zip [0 ..] (concat grid)

  (length $ head grid, length grid, flattened)

adjacentIndices :: Width -> Height -> Grid -> Int -> [Int]
adjacentIndices w h grid i = map (pointToIndex w) indices
  where
    y = i `div` w
    x = i - (w * y)
    indices = filter (\(x, y) -> x >= 0 && x < w && y >= 0 && y < h) [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

getValAtIndex :: Width -> Grid -> Int -> Int
getValAtIndex w grid i = let (_, val) = grid !! i in val

pointToIndex :: Width -> (Int, Int) -> Int
pointToIndex w (x, y) = w * y + x

isLowPoint :: Int -> [Int] -> Bool
isLowPoint p = all (> p)

--------------
-- Start P2
--------------
basinSize :: Width -> Height -> Grid -> Int -> Int
basinSize w h grid i = length $ nub $ mkBasin w h grid [] [i]

mkBasin :: Width -> Height -> Grid -> [Int] -> [Int] -> [Int]
mkBasin _ _ _ _ [] = []
mkBasin w h grid seen ps = ps ++ mkBasin w h grid seen' next'
  where
    adj p = adjacentIndices w h grid p
    vals p = map (getValAtIndex w grid) (adj p)
    next p = [p' | (p', n) <- zip (adj p) (vals p), n < 9] \\ seen'
    seen' = ps ++ seen
    next' = concatMap next ps
