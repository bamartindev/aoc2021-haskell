module Solutions.DayTen
  ( d10p1,
    d10p2,
  )
where

import Data.List (sort)

d10p1 :: [String] -> IO ()
d10p1 input = do
  print $ syntaxErrorScore input

d10p2 :: [String] -> IO ()
d10p2 input = do
  print $ autocompleteScore input

--------------
-- Start P1
--------------

syntaxErrorScore :: [String] -> Int
syntaxErrorScore ls = sum $ map (score . firstInvalidChar []) ls

firstInvalidChar :: [Char] -> String -> Char
firstInvalidChar _ [] = ' '
firstInvalidChar c (x : xs)
  | x `elem` ['(', '[', '{', '<'] = firstInvalidChar (x : c) xs
  | pair (head c) x = firstInvalidChar (tail c) xs
  | otherwise = x
  where
    pair '(' ')' = True
    pair '[' ']' = True
    pair '{' '}' = True
    pair '<' '>' = True
    pair _ _ = False

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = 0

--------------
-- Start P2
--------------

autocompleteScore :: [String] -> Int
autocompleteScore ls = scores !! (length scores `div` 2)
  where
    autocompletes = map (completeLine []) (getIncompleteLines ls)
    scores = sort $ map (foldl (\a x -> (a * 5) + x) 0 . map score') autocompletes

getIncompleteLines :: [String] -> [String]
getIncompleteLines lines = [x | (i, x) <- zip [0 ..] lines, lines' !! i == ' ']
  where
    lines' = map (firstInvalidChar []) lines

completeLine :: [Char] -> String -> [Char]
completeLine stack [] = map flipBracket stack
completeLine stack (x : xs)
  | x `elem` ['(', '[', '{', '<'] = completeLine (x : stack) xs
  | otherwise = completeLine (tail stack) xs

flipBracket :: Char -> Char
flipBracket '(' = ')'
flipBracket '[' = ']'
flipBracket '{' = '}'
flipBracket '<' = '>'
flipBracket n = error $ "Failed to parse character: " ++ show n

score' :: Char -> Int
score' ')' = 1
score' ']' = 2
score' '}' = 3
score' '>' = 4
score' _ = 0
