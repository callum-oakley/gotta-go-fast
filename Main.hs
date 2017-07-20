module Main where

import Brick
import Brick.Widgets.Center (center)
import System.Environment (getArgs)
import Graphics.Vty (Event(..), Key(..), defAttr)

type Name = ()

data State = State { target :: String, input :: String }

cursor :: State -> Location
cursor s = Location (col, row)
  where
    col = textWidth $ takeWhile (/= '\n') $ reverse $ input s
    row = length $ filter (== '\n') $ input s

page :: State -> Widget Name
page s = str $ target s -- TODO split page in to semantic str widgets

draw :: State -> [Widget Name]
draw s = [center $ showCursor () (cursor s) $ page s]

applyChar :: State -> Char -> State
applyChar s c = s { input = input s ++ [c] }

applyBS :: State -> State
applyBS s = s { input = init $ input s }

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey key [])) = case key of
  KChar c -> continue $ applyChar s c
  KEnter -> continue $ applyChar s '\n'
  KBS -> continue $ applyBS s
  KEsc -> halt s
handleEvent s _ = continue s

app :: App State e Name
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  }

main :: IO ()
main = do
  args <- getArgs
  texts <- mapM readFile args
  finalState <- defaultMain app (State { target = head texts, input = "" })
  return ()
