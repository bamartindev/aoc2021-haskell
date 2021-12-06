module Input
  ( readFileByLines,
    readFileOfInts,
    readCommaSeparatedInts,
  )
where

import Data.List.Split (splitOn)
import System.Directory

-- TODO (Brett) Maybe have this function return Either String (IO [String]) or something
-- to avoid it just "crashing" here instead of handling at the top?
readFileByLines :: FilePath -> IO [String]
readFileByLines f = do
  fileExists <- doesFileExist f

  if fileExists
    then do
      content <- readFile f
      pure $ lines content
    else error $ "File Does Not Exist: " ++ f ++ " does not exist!"

readFileOfInts :: FilePath -> IO [Int]
readFileOfInts f = do
  contents <- readFileByLines f
  pure $ map read contents

readCommaSeparatedInts :: FilePath -> IO [Int]
readCommaSeparatedInts f = do
  contents <- readFileByLines f
  pure $ map read $ splitOn "," (head contents)
