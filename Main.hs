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

drawChar :: Maybe Char -> Maybe Char -> Widget Name
drawChar (Just t) (Just i)
  | t == i = str [t]
  | t /= i = str ['X']
drawChar (Just t) Nothing = str [t]
drawChar Nothing (Just i) = str ['Y']

drawLine :: String -> String -> Widget Name
-- We display an empty line as a single space, since str "" takes up no
-- vertical space.
drawLine "" _ = str " "
drawLine ts is = foldl (<+>) emptyWidget charWidgets
  where
    charWidgets = take maxLen $ zipWith drawChar (wrap ts) (wrap is)
    wrap x = map Just x ++ repeat Nothing
    maxLen = max (length ts) (length is)

drawPage :: State -> Widget Name
drawPage s = foldl (<=>) emptyWidget $ lineWidgets
  where
    lineWidgets = zipWith drawLine targetLines (inputLines ++ repeat "")
    targetLines = lines $ target s
    inputLines = lines $ input s

draw :: State -> [Widget Name]
draw s = pure $ center $ showCursor () (cursor s) $ drawPage s

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

run :: String -> IO State
run t = defaultMain app (State { target = t, input = "" })

main :: IO ()
main = do
  args <- getArgs
  targets <- mapM readFile args
  mapM_ run targets
