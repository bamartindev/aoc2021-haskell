module Solutions.D01
  ( d1p1,
    d1p2,
  )
where

d1p1 :: [Int] -> IO ()
d1p1 depths = do
  print $ countLarger depths 0

d1p2 :: [Int] -> IO ()
d1p2 depths = do
  let depthGroups = mkDepthGroups depths
  print $ countLarger depthGroups 0

countLarger :: [Int] -> Int -> Int
countLarger depths@(x : y : _) count
  | x < y = countLarger (tail depths) (count + 1)
  | otherwise = countLarger (tail depths) count
countLarger _ count = count

mkDepthGroups :: [Int] -> [Int]
mkDepthGroups depths@(x : y : z : _) = x + y + z : mkDepthGroups (tail depths)
mkDepthGroups _ = []
