module Main where

import Brick
import System.Environment (getArgs)

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  args <- getArgs
  texts <- mapM readFile args
  mapM_ (simpleMain . ui) texts
