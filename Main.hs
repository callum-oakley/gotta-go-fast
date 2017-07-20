module Main where

import Brick
import Brick.Widgets.Center (center)
import System.Environment (getArgs)
import Graphics.Vty (Event(..), Key(..), defAttr)

type Name = ()

data State = State { _target :: String, _input :: String }

draw :: State -> [Widget Name]
draw s = [center . str $ _target s]

applyChar :: State -> Char -> State
applyChar s c = s { _input = _input s ++ [c] }

applyBS :: State -> State
applyBS s = s { _input = init $ _input s }

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
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  }

main :: IO ()
main = do
  args <- getArgs
  texts <- mapM readFile args
  finalState <- defaultMain app (State { _target = head texts, _input = "" })
  return ()
