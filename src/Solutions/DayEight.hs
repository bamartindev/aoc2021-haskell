module Solutions.DayEight
  ( d8p1,
    d8p2,
  )
where

import Data.List (intersect, sort, subsequences)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

d8p1 :: [String] -> IO ()
d8p1 input = do
  let entries = map parseEntry input
  print $ sum $ map countEasy entries

d8p2 :: [String] -> IO ()
d8p2 input = do
  let entries = map parseEntry input
  let mappings = map mkSimpleMapping entries

  let entries' = zip entries mappings
  let mappings = map (invertMap . completeEntryMap [9, 6, 0, 5, 2, 3]) entries'

  print $ sum $ zipWith (curry decode) entries mappings

type InvertedMapping = M.Map Pattern Int

invertMap :: Mapping -> InvertedMapping
invertMap m = M.fromList im
  where
    im = map (\(a, b) -> (sort b, a)) $ M.toList m

completeEntryMap :: [Int] -> (Entry, Mapping) -> Mapping
completeEntryMap [] (_, m) = m
completeEntryMap n (e, m) = completeEntryMap (tail n) (e, m')
  where
    m' = M.fromList $ findMap e m (head n) : M.toList m

decode :: (Entry, InvertedMapping) -> Int
decode (Entry _ o, m) = foldl addDigit 0 $ map decode' o
  where
    decode' e = case M.lookup (sort e) m of
      Just v -> v
      Nothing -> error $ "Failed to decode! " ++ sort e
    addDigit n d = 10 * n + d

type Pattern = String

data Entry = Entry
  { signalPatterns :: [Pattern],
    output :: [String]
  }
  deriving (Show)

parseEntry :: String -> Entry
parseEntry e = Entry patterns' output'
  where
    [patterns, output] = splitOn " | " e
    patterns' = splitOn " " patterns
    output' = splitOn " " output

countEasy :: Entry -> Int
countEasy (Entry _ output) = length $ filter (\x -> length x `elem` [2, 3, 4, 7]) output

type Mapping = M.Map Int Pattern

mkSimpleMapping :: Entry -> Mapping
mkSimpleMapping (Entry s o) = M.fromList $ filter (\x -> fst x /= -1) $ map m (s ++ o)
  where
    m p = case length p of
      2 -> (1, p)
      3 -> (7, p)
      4 -> (4, p)
      7 -> (8, p)
      _ -> (-1, p)

findMap :: Entry -> Mapping -> Int -> (Int, Pattern)
findMap (Entry s _) m 9 = (9, head [e | e <- s, length e == 6, e `fullyContains` M.findWithDefault "" 4 m])
findMap (Entry s _) m 6 = (6, head [e | e <- s, length e == 6, not $ e `fullyContains` M.findWithDefault "" 7 m])
findMap (Entry s _) m 0 = (0, head [e | e <- s, length e == 6, e `notElem` M.elems m])
findMap (Entry s _) m 5 = (5, head [e | e <- s, length e == 5, M.findWithDefault "" 6 m `fullyContains` e])
findMap (Entry s _) m 2 = (2, head [e | e <- s, length e == 5, M.findWithDefault "" 8 m `fullyContains` e && not (M.findWithDefault "" 9 m `fullyContains` e)])
findMap (Entry s _) m 3 = (3, head [e | e <- s, length e == 5, e `notElem` M.elems m])
findMap _ _ _ = error "Unimplemented map finder"

fullyContains :: String -> String -> Bool
fullyContains a b = b `intersect` a == b && a /= b
