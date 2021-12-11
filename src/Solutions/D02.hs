{-# LANGUAGE ViewPatterns #-}

module Solutions.D02
  ( d2p1,
    d2p2,
  )
where

import Data.List (foldl')

d2p1 :: [String] -> IO ()
d2p1 input = print $ calcHorDep $ map parseCommand input

d2p2 :: [String] -> IO ()
d2p2 input = print $ calcHorDep' $ map parseCommand input

data Command = Forward Int | Down Int | Up Int
  deriving (Show)

parseCommand :: String -> Command
parseCommand (words -> ["forward", read -> n]) = Forward n
parseCommand (words -> ["down", read -> n]) = Down n
parseCommand (words -> ["up", read -> n]) = Up n
parseCommand s = error $ "Could Not Parse: " ++ s

calcHorDep :: [Command] -> Int
calcHorDep = uncurry (*) . foldl' comp (0, 0)
  where
    comp (h, d) (Forward n) = (h + n, d)
    comp (h, d) (Up n) = (h, d - n)
    comp (h, d) (Down n) = (h, d + n)

calcHorDep' :: [Command] -> Int
calcHorDep' xs = let (h, d, _) = foldl' comp (0, 0, 0) xs in h * d
  where
    comp (h, d, a) (Forward n) = (h + n, d + n * a, a)
    comp (h, d, a) (Up n) = (h, d, a - n)
    comp (h, d, a) (Down n) = (h, d, a + n)
