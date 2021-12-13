module Solutions.D13
  ( d13p1,
    d13p2,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Util

d13p1 :: [String] -> IO ()
d13p1 input = do
  let (points, folds) = parseInput input
  print $ length $ fold [head folds] points

d13p2 :: [String] -> IO ()
d13p2 input = do
  let (points, folds) = parseInput input
  printCode $ fold folds points

type PointSet = S.Set (Int, Int)

data Fold = X Int | Y Int
  deriving (Show)

parseInput :: [String] -> (PointSet, [Fold])
parseInput input = (parsePoints ps, parseFolds fs)
  where
    ps = takeWhile (/= "") input
    fs = tail $ dropWhile (/= "") input

parsePoints :: [String] -> PointSet
parsePoints ps = S.fromList $ map (\p -> let [x, y] = splitOn "," p in (read x :: Int, read y :: Int)) ps

parseFolds :: [String] -> [Fold]
parseFolds = map intoFold
  where
    intoFold p =
      let [_, _, dir] = words p
       in case splitOn "=" dir of
            ["x", n] -> X (read n)
            ["y", n] -> Y (read n)
            _ -> error "Parse error on fold"

fold :: [Fold] -> PointSet -> PointSet
fold fs ps = foldl (flip applyFold) ps fs

applyFold :: Fold -> PointSet -> PointSet
applyFold f@(X n) ps = bulkInsert (map (\(x, y) -> (n - (x - n), y)) (foldedPoints f ps)) (bulkDelete (foldedPoints f ps) ps)
applyFold f@(Y n) ps = bulkInsert (map (\(x, y) -> (x, n - (y - n))) (foldedPoints f ps)) (bulkDelete (foldedPoints f ps) ps)

foldedPoints :: Fold -> PointSet -> [(Int, Int)]
foldedPoints (X n) m = S.toList $ S.filter (\(x, _) -> x > n) m
foldedPoints (Y n) m = S.toList $ S.filter (\(_, y) -> y > n) m

bulkInsert :: [(Int, Int)] -> PointSet -> PointSet
bulkInsert xs p = foldl (flip S.insert) p xs

bulkDelete :: [(Int, Int)] -> PointSet -> PointSet
bulkDelete xs p = foldl (flip S.delete) p xs

printCode :: PointSet -> IO ()
printCode ps = do
  putStrLn ""
  mapM_ print (Util.group (fst max + 1) drawn)
  where
    drawn = map (\p -> if S.member p ps then '#' else ' ') grid
    grid = [(x, y) | y <- [0 .. snd max], x <- [0 .. fst max]]
    max = (maximum (map fst ls), maximum (map snd ls))
    ls = S.toList ps
