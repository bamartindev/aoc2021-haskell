module Solutions.DayFive
  ( d5p1,
    d5p2,
  )
where

import Data.List.Split (splitOn)
-- Grabbed the idea from others to use Map to group the similar points after generation.
-- Was trying to do a list comprehension and well....yeah that would have finished by
-- the time AoC 2022 was starting.
import qualified Data.Map.Strict as M

d5p1 :: [String] -> IO ()
d5p1 input = do
  let lines = map parseLine input
  let points = concatMap (genPoints PartOne) lines

  print $ countOverlap $ group points

d5p2 :: [String] -> IO ()
d5p2 input = do
  let lines = map parseLine input
  let points = concatMap (genPoints PartTwo) lines

  print $ countOverlap $ group points

type Point = (Int, Int)

data Line = Line Int Int Int Int

data Part = PartOne | PartTwo

parseLine :: String -> Line
parseLine s = do
  let [p1, _, p2] = words s
  let [x1, y1] = splitOn "," p1
  let [x2, y2] = splitOn "," p2
  Line (read x1) (read y1) (read x2) (read y2)

genPoints :: Part -> Line -> [Point]
genPoints part l@(Line x1 y1 x2 y2)
  | x1 == x2 && y1 < y2 = zip (repeat x1) [y1 .. y2]
  | x1 == x2 && y2 < y1 = zip (repeat x1) [y2 .. y1]
  | y1 == y2 && x1 < x2 = zip [x1 .. x2] (repeat y1)
  | y1 == y2 && x2 < x1 = zip [x2 .. x1] (repeat y1)
  | otherwise = case part of
    PartOne -> []
    PartTwo -> diag l

diag :: Line -> [Point]
diag (Line x1 y1 x2 y2)
  | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
  | x1 > x2 && y1 < y2 = zip (reverse [x2 .. x1]) [y1 .. y2]
  | x1 < x2 && y1 > y2 = zip [x1 .. x2] (reverse [y2 .. y1])
  | x1 > x2 && y1 > y2 = zip (reverse [x2 .. x1]) (reverse [y2 .. y1])
  | otherwise = error "Failed to parse"

group :: (Ord k, Num a) => [k] -> M.Map k a
group p = M.fromListWith (+) (zip p (repeat 1))

countOverlap :: M.Map k Integer -> Int
countOverlap = M.size . M.filter (> 1)
