{-# LANGUAGE OverloadedStrings #-}

module Solutions.DayFour
  ( d4p1,
    d4p2,
    findNumPos,
  )
where

import Data.List
import qualified Data.Text as T

type Board = [[String]]

type MarkBoard = [[Bool]]

type BoardPair = (Board, MarkBoard)

d4p1 :: [String] -> IO ()
d4p1 input = do
  let numbers = parseDrawNums input
  let boards = parseBoards $ filter (/= "") $ tail input

  let winner = findFirstWinner numbers boards
  print $ score winner

d4p2 :: [String] -> IO ()
d4p2 input = do
  let numbers = parseDrawNums input
  let boards = parseBoards $ filter (/= "") $ tail input

  let lastWinner = findLastWinner numbers boards
  print $ score lastWinner

-- Going String to Text to String seems a bit bad but oh well
parseDrawNums :: [String] -> [String]
parseDrawNums input = map T.unpack $ T.splitOn "," $ T.pack $ head input

mkEmptyMarkBoard :: MarkBoard
mkEmptyMarkBoard = replicate 5 (replicate 5 False)

parseBoards :: [String] -> [BoardPair]
parseBoards [] = []
parseBoards input = (map words as, mkEmptyMarkBoard) : parseBoards bs
  where
    (as, bs) = splitAt 5 input

score :: (BoardPair, Int) -> Int
score (bp, n) = n * sum (unmarked bp)

unmarked :: BoardPair -> [Int]
unmarked (b, m) = map f $ filter (not . snd) $ zip (concat b) (concat m)
  where
    f (v, _) = read v

findFirstWinner :: [String] -> [BoardPair] -> (BoardPair, Int)
findFirstWinner [] _ = error "No winner found!"
findFirstWinner (n : ns) bs = case find isWinner next of
  Just bp -> (bp, read n)
  Nothing -> findFirstWinner ns next
  where
    next = map (markNumber n) bs

findLastWinner :: [String] -> [BoardPair] -> (BoardPair, Int)
findLastWinner [] _ = error "Last winner not found!"
findLastWinner (n : ns) bs = case filter (not . isWinner) next of
  [] -> (head next, read n) -- When the list is empty that means the winner was found, "rewind" to get te board from next
  xs -> findLastWinner ns xs
  where
    next = map (markNumber n) bs

isWinner :: BoardPair -> Bool
isWinner (b, m) = hor || vert
  where
    hor = any (all (== True)) m
    vert = any (all (== True)) (transpose m)

markNumber :: String -> BoardPair -> BoardPair
markNumber n (b, m) = (b, m')
  where
    m' = case findNumPos n b of
      [(x, y)] -> replace2D x y True m
      _ -> m

findNumPos :: String -> Board -> [(Int, Int)]
findNumPos n b = [(x, y) | (x, row) <- enumerate b, (y, val) <- enumerate row, val == n]

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

replace :: Int -> a -> [a] -> [a]
replace i val xs = do
  let (front, _ : back) = splitAt i xs
  front ++ val : back

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D row col x xs =
  let rowToReplaceIn = xs !! row
      modifiedRow = replace col x rowToReplaceIn
   in replace row modifiedRow xs
