module Main where

import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Random (randomRIO)

import UI

maxLineLength :: Int
maxLineLength = 80

maxNoOfLines :: Int
maxNoOfLines = 30

trimEmptyLines :: [String] -> [String]
trimEmptyLines = reverse . dropWhile (== "") . reverse . dropWhile (== "")

-- TODO wrap lines instead of cutting them off at maxLineLength
strip :: String -> IO String
strip file = do
  let ls = map (take maxLineLength) $ lines file -- wrap at this point
  -- For files longer than maxNoOfLines we grab a random segment.
  r <- randomRIO (0, max 0 $ length ls - maxNoOfLines)
  return $ unlines $ trimEmptyLines $ take maxNoOfLines $ drop r ls

main :: IO ()
main = do
  args <- getArgs
  files <- filterM doesFileExist args
  case files of
    [] -> do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " <file(s)>"
    _ -> do
      r <- randomRIO (0, length files - 1)
      file <- readFile $ files !! r
      stripped <- strip file
      run stripped
