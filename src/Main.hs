{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (filterM)
import System.Console.CmdArgs
  (Data, Typeable, args, cmdArgs, def, help, program, summary, typ, (&=))
import System.Directory (doesFileExist)
import System.Random (randomRIO)

import UI (run)
import FormatCode (expandTabs, trimEmptyLines, wordWrap)

data Config = Config
  { height :: Int
  , width :: Int
  , tab :: Int
  , files :: [FilePath]
  } deriving (Show, Data, Typeable)

config :: Config
config = Config
  { height = 20 &= typ "LINES" &=
    help "The maximum number of lines to sample (default: 20)"
  , width = 80 &= typ "CHARS" &=
    help "The width at which to wrap lines (default: 80)"
  , tab = 4 &= typ "SIZE" &=
    help "The size of a tab in spaces (default: 4)"
  , files = def &= args &= typ "FILES"
  }
  &= summary "Gotta Go Fast 0.1.0.0"
  &= help "Practice typing and measure your WPM and accuracy"
  &= program "gotta-go-fast"

sample :: Config -> String -> IO String
sample c file = do
  let ls = lines $ wordWrap (width c) $ expandTabs (tab c) file
  -- For files longer than `lines c` we grab a random segment.
  r <- randomRIO (0, max 0 $ length ls - height c)
  return $ unlines $ trimEmptyLines $ take (height c) $ drop r ls

main :: IO ()
main = do
  c <- cmdArgs config
  fs <- filterM doesFileExist $ files c
  case fs of
    [] -> putStrLn $ "Requires at least one file path"
    _ -> do
      r <- randomRIO (0, length fs - 1)
      file <- readFile $ fs !! r
      sampled <- sample c file
      run sampled
