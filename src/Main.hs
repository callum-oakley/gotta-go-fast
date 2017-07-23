module Main where

import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Random (randomRIO)

import UI
import FormatCode (expandTabs, wordWrap)

maxLineLength :: Int
maxLineLength = 80

maxNoOfLines :: Int
maxNoOfLines = 30

tabWidth :: Int
tabWidth = 4

trimEmptyLines :: [String] -> [String]
trimEmptyLines = reverse . dropWhile (== "") . reverse . dropWhile (== "")

sample :: String -> IO String
sample file = do
  let ls = lines $ wordWrap maxLineLength $ expandTabs tabWidth file
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
      sampled <- sample file
      run sampled
