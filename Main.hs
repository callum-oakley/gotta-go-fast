module Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!!! :o"

main :: IO ()
main = simpleMain ui
