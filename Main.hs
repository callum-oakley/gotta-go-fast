module Main where

import Debug.Trace

import Brick
import Brick.Widgets.Center (center)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.Time
  ( NominalDiffTime
  , UTCTime
  , diffTimeToPicoseconds
  , diffUTCTime
  , getCurrentTime
  )
import Graphics.Vty
  ( Event(..)
  , Key(..)
  , Modifier(..)
  , brightBlack
  , defAttr
  , red
  )
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

type Name = ()

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data RelativeLocation = BeforeCursor | AfterCursor

data State = State
  { target :: String
  , input :: String
  , start :: Maybe UTCTime
  , end :: Maybe UTCTime
  , strokes :: Integer
  , hits :: Integer
  }

targetAttr :: AttrName
targetAttr = attrName "target"

errorAttr :: AttrName
errorAttr = attrName "error"

hasEnded :: State -> Bool
hasEnded = isJust . end

hasStarted :: State -> Bool
hasStarted = isJust . start

cursorCol :: State -> Int
cursorCol = textWidth . takeWhile (/= '\n') . reverse . input

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input

cursor :: State -> Location
cursor s = Location (cursorCol s, cursorRow s)

visibleSpace :: Char -> Char
visibleSpace ' ' = '_'
visibleSpace c = c

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
    linePairs = zip (lines $ target s) ((lines $ input s) ++ repeat "")

drawResults :: State -> Widget Name
drawResults s =
  (str $ "You typed " ++ x ++ " characters in " ++ y ++ " seconds.") <=>
  (str " ") <=>
  (str $ "Words per minute: " ++ show (round wpm)) <=>
  (str " ") <=>
  (str $ "Accuracy: " ++ show (round accuracy) ++ "%")
  where
    x = show $ length $ target s
    y = show $ round seconds
    wpm = (fromIntegral $ length $ target s) / (5 * seconds / 60)
    seconds = toRational $ diffUTCTime (fromJust $ end s) (fromJust $ start s)
    accuracy = (fromIntegral $ hits s) / (fromIntegral $ strokes s) * 100

draw :: State -> [Widget Name]
draw s
  | hasEnded s = pure $ center $ drawResults s
  | otherwise = pure $ center $ showCursor () (cursor s) $ drawPage s

isHit :: State -> Char -> Bool
isHit s c = isPrefixOf (input s ++ [c]) (target s)

applyChar :: Char -> State -> State
applyChar c s = s
  { input = input s ++ [c]
  , strokes = strokes s + 1
  , hits = hits s + if isHit s c then 1 else 0
  }

backspace :: String -> String
backspace "" = ""
backspace xs = init xs

applyBackspace :: State -> State
applyBackspace s = s { input = backspace $ input s }

backspaceWord :: String -> String
backspaceWord xs = reverse $ dropWhile (/= ' ') $ reverse $ backspace xs

applyBackspaceWord :: State -> State
applyBackspaceWord s = s { input = backspaceWord $ input s }

handleEnter :: State -> EventM Name (Next State)
handleEnter s
  | hasEnded s = halt s
  | cursorRow s < (length $ lines $ target s) - 1 = continue $ applyChar '\n' s
  | (input s) ++ "\n" == target s = do
    now <- liftIO getCurrentTime
    continue $ s { end = Just now }
  | otherwise = continue s

handleChar :: Char -> State -> EventM Name (Next State)
handleChar c s
  | hasStarted s = continue $ applyChar c s
  | otherwise = do
    now <- liftIO getCurrentTime
    continue $ applyChar c $ s { start = Just now }

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey key [])) = case key of
  KChar c -> handleChar c s
  KEnter -> handleEnter s
  KBS -> continue $ applyBackspace s
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

initialState :: String -> State
initialState target = State
  { target = target
  , input = ""
  , start = Nothing
  , end = Nothing
  , strokes = 0
  , hits = 0
  }

-- TODO use terminal heights and widths here, or make configurable
-- TODO wrap lines instead of trimming them
-- TODO take a random sample, not just the first n lines
strip :: String -> String
strip = unlines . take 30 . map (take 80) . lines

main :: IO ()
main = do
  args <- getArgs
  -- TODO open a random file in the directory if none are given
  raw <- mapM readFile args
  let targets = map strip raw
  mapM_ (defaultMain app . initialState) targets
