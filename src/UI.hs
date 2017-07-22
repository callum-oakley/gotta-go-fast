module UI where

import Brick
  ( App(..), AttrName, BrickEvent(..), EventM, Location(..), Next, Widget
  , attrMap, attrName, continue, defaultMain, emptyWidget, fg, halt, showCursor
  , showFirstCursor, str, withAttr, (<+>), (<=>)
  )
import Brick.Widgets.Center (center)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Graphics.Vty
  (Event(..), Key(..), Modifier(..), brightBlack, defAttr, red)

import GottaGoFast

type Name = ()

emptyAttr :: AttrName
emptyAttr = attrName "target"

missAttr :: AttrName
missAttr = attrName "error"

drawCharacter :: Character -> Widget Name
drawCharacter (Hit c) = str [c]
drawCharacter (Miss ' ') = withAttr missAttr $ str ['_']
drawCharacter (Miss c) = withAttr missAttr $ str [c]
drawCharacter (Empty c) = withAttr emptyAttr $ str [c]

drawLine :: Line -> Widget Name
-- We display an empty line as a single space so that it still occupies
-- vertical space.
drawLine [] = str " "
drawLine ls = foldl1 (<+>) $ map drawCharacter ls

drawPage :: State -> Widget Name
drawPage s = foldl (<=>) emptyWidget $ map drawLine $ page s

drawResults :: State -> Widget Name
drawResults s = str $
  "You typed " ++ x ++ " characters in " ++ y ++ " seconds.\n\n" ++
  "Words per minute: " ++ show (round $ wpm s) ++ "\n\n" ++
  "Accuracy: " ++ show (round $ accuracy s * 100) ++ "%"
  where
    x = show $ length $ target s
    y = show $ round $ seconds s

draw :: State -> [Widget Name]
draw s
  | hasEnded s = pure $ center $ drawResults s
  | otherwise = pure $ center $ showCursor () (Location $ cursor s) $ drawPage s

handleEnter :: State -> EventM Name (Next State)
handleEnter s
  | hasEnded s = halt s
  | not $ onLastLine s = continue $ applyEnter s
  | input s ++ "\n" == target s = do
    now <- liftIO getCurrentTime
    continue $ stopClock now s
  | otherwise = continue s

handleChar :: Char -> State -> EventM Name (Next State)
handleChar c s
  | hasStarted s = continue $ applyChar c s
  | otherwise = do
    now <- liftIO getCurrentTime
    continue $ applyChar c $ startClock now s

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey key [])) =
  case key of
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

app :: App State e Name
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $
      attrMap defAttr [(emptyAttr, fg brightBlack), (missAttr, fg red)]
  }

run :: String -> IO ()
run t = do
  _ <- defaultMain app $ initialState t
  return ()
