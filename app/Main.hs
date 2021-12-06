module Main where

import qualified Input
import qualified Solutions
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  runWithArgs args

usage :: String
usage = "Usage: aoc2021-haskell [day] [part]"

type Day = Int

type Part = Int

type Solution = (Day, Part)

-- TODO (Brett) - I wonder if there is a "flatter" way to implement this?

-- | mkSolution takes a string representation of a day and part, then parses
-- the String into Int in order to maybe return a Solution
mkSolution :: String -> String -> Maybe Solution
mkSolution day part = case readMaybe day :: Maybe Int of
  Just day -> case readMaybe part :: Maybe Int of
    Just part -> Just (day, part)
    Nothing -> Nothing
  Nothing -> Nothing

showSolutionMetadata :: Solution -> IO ()
showSolutionMetadata solution = putStr $ "Day " ++ show (fst solution) ++ ", Part " ++ show (snd solution) ++ ": "

-- | runWithArgs takes an array of Strings and trys to extract a day, or day and part from them.
-- If able to extract the day / part then run the requested solution.
runWithArgs :: [String] -> IO ()
runWithArgs [] = putStrLn usage
runWithArgs [day] = do
  let maybeSolutionP1 = mkSolution day "1"
  let maybeSolutionP2 = mkSolution day "2"

  case maybeSolutionP1 of
    Nothing -> putStrLn "Failed to parse day make sure it is an integers (1, 2, 3, etc.)"
    Just solution -> runSolution solution

  case maybeSolutionP2 of
    Nothing -> putStrLn "Failed to parse day make sure it is an integers (1, 2, 3, etc.)"
    Just solution -> runSolution solution
runWithArgs [day, part] = do
  let maybeSolution = mkSolution day part
  case maybeSolution of
    Nothing -> putStrLn "Failed to parse arguments into day and part, make sure they are integers (1, 2, 3, etc.)"
    Just solution -> runSolution solution
runWithArgs _ = putStrLn usage

-- | runSolution takes a Solution and attempts to run the function that implements the solution, if it is implemented
runSolution :: Solution -> IO ()
runSolution solution = case solution of
  (1, 1) -> do
    showSolutionMetadata solution
    input <- Input.readFileOfInts "inputs/day-1.txt"
    Solutions.d1p1 input
  (1, 2) -> do
    showSolutionMetadata solution
    input <- Input.readFileOfInts "inputs/day-1.txt"
    Solutions.d1p2 input
  (2, 1) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-2.txt"
    Solutions.d2p1 input
  (2, 2) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-2.txt"
    Solutions.d2p2 input
  (3, 1) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-3.txt"
    Solutions.d3p1 input
  (3, 2) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-3.txt"
    Solutions.d3p2 input
  (4, 1) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-4.txt"
    Solutions.d4p1 input
  (4, 2) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-4.txt"
    Solutions.d4p2 input
  (5, 1) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-5.txt"
    Solutions.d5p1 input
  (5, 2) -> do
    showSolutionMetadata solution
    input <- Input.readFileByLines "inputs/day-5.txt"
    Solutions.d5p2 input
  (6, 1) -> do
    showSolutionMetadata solution
    input <- Input.readCommaSeparatedInts "inputs/day-6.txt"
    Solutions.d6p1 input
  (6, 2) -> do
    showSolutionMetadata solution
    input <- Input.readCommaSeparatedInts "inputs/day-6.txt"
    Solutions.d6p2 input
  _ -> putStrLn $ "No solution implemented for day " ++ show (fst solution) ++ ", part" ++ show (snd solution)
