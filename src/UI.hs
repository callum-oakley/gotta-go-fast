module UI (run) where

import Brick
  ( App(..), AttrName, BrickEvent(..), EventM, Location(..), Next, Widget
  , attrMap, attrName, continue, defaultMain, emptyWidget, fg, halt, padAll
  , showCursor, showFirstCursor, str, withAttr, (<+>), (<=>)
  )
import Brick.Widgets.Border (border, borderAttr)
import Brick.Widgets.Center (center)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Graphics.Vty
  (Event(..), Key(..), Modifier(..), brightBlack, defAttr, red)

import GottaGoFast

emptyAttr :: AttrName
emptyAttr = attrName "target"

missAttr :: AttrName
missAttr = attrName "error"

drawCharacter :: Character -> Widget ()
drawCharacter (Hit c) = str [c]
drawCharacter (Miss ' ') = withAttr missAttr $ str ['_']
drawCharacter (Miss c) = withAttr missAttr $ str [c]
drawCharacter (Empty c) = withAttr emptyAttr $ str [c]

drawLine :: Line -> Widget ()
-- We display an empty line as a single space so that it still occupies
-- vertical space.
drawLine [] = str " "
drawLine ls = foldl1 (<+>) $ map drawCharacter ls

drawPage :: State -> Widget ()
drawPage s = foldl (<=>) emptyWidget $ map drawLine $ page s

drawResults :: State -> Widget ()
drawResults s = str $
  "You typed " ++ x ++ " characters in " ++ y ++ " seconds.\n\n" ++
  "Words per minute: " ++ show (round $ wpm s) ++ "\n\n" ++
  "Accuracy: " ++ show (round $ accuracy s * 100) ++ "%"
  where
    x = show $ noOfChars s
    y = show $ round $ seconds s

draw :: State -> [Widget ()]
draw s
  | hasEnded s = pure $ center $ drawResults s
  | isErrorFree s = pure $ center p
  | otherwise = pure $ center $ border p
  where
    p = padAll 1 $ showCursor () (Location $ cursor s) $ drawPage s

handleEnter :: State -> EventM () (Next State)
handleEnter s
  | hasEnded s = halt s
  | not $ onLastLine s = continue $ applyEnter s
  | isComplete $ applyChar '\n' s = do
    now <- liftIO getCurrentTime
    continue $ stopClock now s
  | otherwise = continue s

handleChar :: Char -> State -> EventM () (Next State)
handleChar c s
  | c == ' ' && atEndOfLine s = handleEnter s
  | hasStarted s = continue $ applyChar c s
  | otherwise = do
    now <- liftIO getCurrentTime
    continue $ applyChar c $ startClock now s

handleEvent :: State -> BrickEvent () e -> EventM () (Next State)
handleEvent s (VtyEvent (EvKey key [])) =
  case key of
    KChar '\t' -> continue $ applyTab s
    KChar c -> handleChar c s
    KEnter -> handleEnter s
    KBS -> continue $ applyBackspace s
    _ -> continue s
handleEvent s (VtyEvent (EvKey key [MCtrl])) =
  case key of
    KChar 'w' -> continue $ applyBackspaceWord s
    KChar 'c' -> halt s
    KChar 'd' -> halt s
    _ -> continue s
handleEvent s _ = continue s

app :: App State e ()
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr
    [(emptyAttr, fg brightBlack), (missAttr, fg red), (borderAttr, fg red)]
  }

run :: String -> IO ()
run t = do
  _ <- defaultMain app $ initialState t
  return ()
