module Main where

import Brick
import Brick.Widgets.Center (center)
import Data.Monoid ((<>))
import Graphics.Vty
  ( Event(..)
  , Key(..)
  , Modifier(..)
  , brightBlack
  , defAttr
  , red
  )
import System.Environment (getArgs)

type Name = ()

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data RelativeLocation = BeforeCursor | AfterCursor

data State = State { target :: String, input :: String }

targetAttr :: AttrName
targetAttr = attrName "target"

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
  | t /= i && i == ' ' = withAttr errorAttr $ str ['_']
  | otherwise = withAttr errorAttr $ str [i]
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

backspace :: String -> String
backspace "" = ""
backspace xs = init xs

applyBackspace :: State -> State
applyBackspace s = s { input = backspace $ input s }

backspaceWord :: String -> String
backspaceWord xs = reverse $ dropWhile (/= ' ') $ reverse $ backspace xs

applyBackspaceWord :: State -> State
applyBackspaceWord s = s { input = backspaceWord $ input s }

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey key [])) = case key of
  KChar c -> continue $ applyChar s c
  KEnter -> continue $ applyChar s '\n'
  KBS -> continue $ applyBackspace s
  KEsc -> halt s
  _ -> continue s
handleEvent s (VtyEvent (EvKey key [MCtrl])) = case key of
  KChar 'w' -> continue $ applyBackspaceWord s
  KChar 'c' -> halt s
  KChar 'd' -> halt s
  _ -> continue s
handleEvent s _ = continue s

app :: App State e Name
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr
    [(targetAttr, fg brightBlack), (errorAttr, fg red)]
  }

run :: String -> IO State
run t = defaultMain app (State { target = t, input = "" })

-- TODO use terminal heights and widths here, or make configurable
strip :: String -> String
strip = unlines . take 50 . map (take 80) . lines

main :: IO ()
main = do
  args <- getArgs
  raw <- mapM readFile args
  let targets = map strip raw
  mapM_ run targets
