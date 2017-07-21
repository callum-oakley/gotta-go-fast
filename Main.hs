module Main where

import Brick
import Brick.Widgets.Center (center)
import System.Environment (getArgs)
import Graphics.Vty (Event(..), Key(..), defAttr, brightBlack, red)

type Name = ()

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data RelativeLocation = BeforeCursor | AfterCursor

data State = State { target :: String, input :: String }

targetAttr :: AttrName
targetAttr = attrName "base"

errorAttr :: AttrName
errorAttr = attrName "error"

cursorCol :: State -> Int
cursorCol = textWidth . takeWhile (/= '\n') . reverse . input

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input

cursor :: State -> Location
cursor s = Location (cursorCol s, cursorRow s)

drawChar :: RelativeLocation -> (Maybe Char, Maybe Char) -> Widget Name
drawChar _ (Just t, Just i)
  | t == i = str [t]
  | t /= i = withAttr errorAttr $ str [i]
drawChar _ (Nothing, Just i) = withAttr errorAttr $ str [i]
drawChar BeforeCursor (Just t, Nothing) = withAttr errorAttr $ str [t]
drawChar AfterCursor (Just t, Nothing) = withAttr targetAttr $ str [t]

drawLine :: RelativeLocation -> (String, String) -> Widget Name
-- We display an empty line as a single space, since str "" takes up no
-- vertical space. This doesn't affect input in any way.
drawLine _ ("", "") = str " "
drawLine rl (ts, is) = foldl (<+>) emptyWidget charWidgets
  where
    charWidgets = map (drawChar rl) charPairs
    charPairs = take maxLen $ zip (nothingsForever ts) (nothingsForever is)
    nothingsForever x = map Just x ++ repeat Nothing
    maxLen = max (length ts) (length is)

drawPage :: State -> Widget Name
drawPage s = foldl (<=>) emptyWidget $ lineWidgetsBC ++ lineWidgetsAC
  where
    lineWidgetsBC = map (drawLine BeforeCursor) $ take (cursorRow s) linePairs
    lineWidgetsAC = map (drawLine AfterCursor) $ drop (cursorRow s) linePairs
    linePairs = zip (lines $ target s) ((lines $ input s) ++ repeat "")

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
  , appAttrMap = const $ attrMap defAttr
    [ (targetAttr, fg brightBlack)
    , (errorAttr, fg red)
    ]
  }

run :: String -> IO State
run t = defaultMain app (State { target = t, input = "" })

main :: IO ()
main = do
  args <- getArgs
  targets <- mapM readFile args
  mapM_ run targets
