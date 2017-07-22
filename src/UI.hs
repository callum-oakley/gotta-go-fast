module UI where

import Brick
  ( App(..), AttrName, BrickEvent(..), EventM, Location(..), Next, Widget
  , attrMap, attrName, continue, defaultMain, emptyWidget, fg, halt, showCursor
  , showFirstCursor, str, withAttr, (<+>), (<=>)
  )
import Brick.Widgets.Center (center)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Time (diffUTCTime, getCurrentTime)
import Graphics.Vty
  (Event(..), Key(..), Modifier(..), brightBlack, defAttr, red)

import GottaGoFast

type Name = ()

targetAttr :: AttrName
targetAttr = attrName "target"

errorAttr :: AttrName
errorAttr = attrName "error"

visibleSpace :: Char -> Char
visibleSpace ' ' = '_'
visibleSpace c  = c

drawChar :: RelativeLocation -> (Maybe Char, Maybe Char) -> Widget Name
drawChar _ (Just t, Just i)
  | t == i = str [t]
  | t /= i = withAttr errorAttr $ str [visibleSpace i]
drawChar _ (Nothing, Just i) = withAttr errorAttr $ str [visibleSpace i]
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
    linePairs = zip (lines $ target s) (lines (input s) ++ repeat "")

drawResults :: State -> Widget Name
drawResults s =
  str ("You typed " ++ x ++ " characters in " ++ y ++ " seconds.") <=>
  str " " <=>
  str ("Words per minute: " ++ show (round wpm)) <=>
  str " " <=>
  str ("Accuracy: " ++ show (round accuracy) ++ "%")
  where
    x = show $ length $ target s
    y = show $ round seconds
    wpm = fromIntegral (length $ target s) / (5 * seconds / 60)
    seconds = toRational $ diffUTCTime (fromJust $ end s) (fromJust $ start s)
    accuracy = fromIntegral (hits s) / fromIntegral (strokes s) * 100

draw :: State -> [Widget Name]
draw s
  | hasEnded s = pure $ center $ drawResults s
  | otherwise = pure $ center $ showCursor () (Location $ cursor s) $ drawPage s

handleEnter :: State -> EventM Name (Next State)
handleEnter s
  | hasEnded s = halt s
  | cursorRow s < length (lines $ target s) - 1 = continue $ applyChar '\n' s
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
      attrMap defAttr [(targetAttr, fg brightBlack), (errorAttr, fg red)]
  }

run :: String -> IO ()
run t = do
  _ <- defaultMain app $ initialState t
  return ()
