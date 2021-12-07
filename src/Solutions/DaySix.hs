module Solutions.DaySix
  ( d6p1,
    d6p2,
  )
where

d6p1 :: [Int] -> IO ()
d6p1 input = print $ sim 80 (mkFish input)

d6p2 :: [Int] -> IO ()
d6p2 input = print $ sim 256 (mkFish input)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

mkFish :: [Int] -> [Int]
mkFish input = [count x input | x <- [0 .. 8]]

sim :: Int -> [Int] -> Int
sim c f = sum $ iterate next f !! c

next :: [Int] -> [Int]
next [d0, d1, d2, d3, d4, d5, d6, d7, d8] = [d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0]
next _ = error "Need 9 elements in list"
