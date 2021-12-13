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
  print $ length $ fold points [head folds]

d13p2 :: [String] -> IO ()
d13p2 input = do
  let (points, folds) = parseInput input
  printCode $ fold points folds

type PointSet = S.Set (Int, Int)

data Fold = X Int | Y Int
  deriving (Show)

parseInput :: [String] -> (PointSet, [Fold])
parseInput input = let [ps, fs] = splitOn [""] input in (parsePoints ps, parseFolds fs)

parsePoints :: [String] -> PointSet
parsePoints ps = S.fromList $ map (\p -> let [x, y] = splitOn "," p in (read x, read y)) ps

parseFolds :: [String] -> [Fold]
parseFolds = map intoFold
  where
    intoFold p = case splitOn "=" (last $ words p) of
      ["x", n] -> X (read n)
      ["y", n] -> Y (read n)
      _ -> error "Parse error on fold"

fold :: PointSet -> [Fold] -> PointSet
fold = foldl applyFold

applyFold :: PointSet -> Fold -> PointSet
applyFold ps f@(X n) = S.map (\p@(x, y) -> if x > n then (n - (x - n), y) else p) ps
applyFold ps f@(Y n) = S.map (\p@(x, y) -> if y > n then (x, n - (y - n)) else p) ps

printCode :: PointSet -> IO ()
printCode ps = do
  putStrLn ""
  mapM_ print (Util.group (fst max + 1) drawn)
  where
    drawn = map (\p -> if S.member p ps then '#' else ' ') grid
    grid = [(x, y) | y <- [0 .. snd max], x <- [0 .. fst max]]
    max = (maximum (map fst ls), maximum (map snd ls))
    ls = S.toList ps
