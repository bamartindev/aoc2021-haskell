module Solutions.D12
  ( d12p1,
    d12p2,
  )
where

import Data.Char (isLower, isUpper)
import Data.List (group, nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

d12p1 :: [String] -> IO ()
d12p1 input = do
  let m = mkAdjMap $ parsePairs input
  print $ length $ traverse' m []

d12p2 :: [String] -> IO ()
d12p2 input = do
  let m = mkAdjMap $ parsePairs input
  print $ length $ traverse'' m []

type Pair = (String, String)

parsePairs :: [String] -> [Pair]
parsePairs = map (\s -> let [a, b] = splitOn "-" s in (a, b))

other :: String -> Pair -> String
other current (a, b) = if a == current then b else a

labels :: [Pair] -> [String]
labels ps = nub $ concatMap (\(a, b) -> [a, b]) ps

mkAdjMap :: [Pair] -> M.Map String [String]
mkAdjMap ps = M.fromList $ zip lbls (map (adj ps) lbls)
  where
    lbls = labels ps

adj :: [Pair] -> String -> [String]
adj ps current = map (other current) $ filter (\(a, b) -> a == current || b == current) ps

traverse' :: M.Map String [String] -> [String] -> [[String]]
traverse' m [] = traverse' m ["start"]
traverse' m seen@(c : cs)
  | c == "end" = [seen]
  | otherwise = case M.lookup c m of
    Just adjs -> concatMap (traverse' m . (: seen)) (next adjs)
    Nothing -> error "Not really sure how I got here :)"
  where
    next :: [String] -> [String]
    next adjs = filter (\n -> isUpper (head n) || n `notElem` cs) adjs

-- Was seeing if I could have a PartOne PartTwo type to pass this
-- but getting to the point where I could map traverse was more
-- difficult than anticipated.  Easier to just copy and modify ;)
traverse'' :: M.Map String [String] -> [String] -> [[String]]
traverse'' m [] = traverse'' m ["start"]
traverse'' m seen@(c : cs)
  | c == "end" = [seen]
  | otherwise = case M.lookup c m of
    Just adjs -> concatMap (traverse'' m . (: seen)) (next adjs)
    Nothing -> error "Not really sure how I got here :)"
  where
    next :: [String] -> [String]
    next adjs = filter pred adjs

    pred :: String -> Bool
    pred "start" = False
    pred n = isUpper (head n) || not dbl || n `notElem` cs

    dbl :: Bool
    dbl = any (\x -> length x == 2) $ group $ sort $ filter (isLower . head) seen
