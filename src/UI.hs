module UI
  ( run
  ) where

import           Brick                  (App (..), AttrName, BrickEvent (..),
                                         EventM, Location (..), Next,
                                         Padding (..), Widget, attrMap,
                                         attrName, continue, defaultMain,
                                         emptyWidget, fg, halt, padAll,
                                         padBottom, showCursor, showFirstCursor,
                                         str, withAttr, (<+>), (<=>))
import           Brick.Widgets.Center   (center)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Graphics.Vty           (Attr, Color (..), Event (..), Key (..),
                                         Modifier (..), bold, defAttr,
                                         withStyle)

import           GottaGoFast

emptyAttrName :: AttrName
emptyAttrName = attrName "empty"

errorAttrName :: AttrName
errorAttrName = attrName "error"

resultAttrName :: AttrName
resultAttrName = attrName "result"

drawCharacter :: Character -> Widget ()
drawCharacter (Hit c)    = str [c]
drawCharacter (Miss ' ') = withAttr errorAttrName $ str ['_']
drawCharacter (Miss c)   = withAttr errorAttrName $ str [c]
drawCharacter (Empty c)  = withAttr emptyAttrName $ str [c]

drawLine :: Line -> Widget ()
-- We display an empty line as a single space so that it still occupies
-- vertical space.
drawLine [] = str " "
drawLine ls = foldl1 (<+>) $ map drawCharacter ls

drawText :: State -> Widget ()
drawText s = padBottom (Pad 2) . foldl (<=>) emptyWidget . map drawLine $ page s

drawResults :: State -> Widget ()
drawResults s =
  withAttr resultAttrName . str $
  (show . round $ wpm s) ++
  " words per minute • " ++ (show . round $ accuracy s * 100) ++ "% accuracy"

draw :: State -> [Widget ()]
draw s
  | hasEnded s = pure . center . padAll 1 $ drawText s <=> drawResults s
  | otherwise =
    pure . center . padAll 1 . showCursor () (Location $ cursor s) $
    drawText s <=> str " "

handleChar :: Char -> State -> EventM () (Next State)
handleChar c s
  | isSpace c && isComplete (applyWhitespace s) = do
    now <- liftIO getCurrentTime
    continue $ stopClock now s
  | isSpace c = continue $ applyWhitespace s
  | hasStarted s = continue $ applyChar c s
  | otherwise = do
    now <- liftIO getCurrentTime
    continue . applyChar c $ startClock now s

handleEvent :: State -> BrickEvent () e -> EventM () (Next State)
handleEvent s (VtyEvent (EvKey key [MCtrl])) =
  case key of
    KChar 'c' -> halt s
    KChar 'd' -> halt s
    KChar 'w' -> continue $ applyBackspaceWord s
    KBS       -> continue $ applyBackspaceWord s
    _         -> continue s
handleEvent s (VtyEvent (EvKey key [MAlt])) =
  case key of
    KBS -> continue $ applyBackspaceWord s
    _   -> continue s
handleEvent s (VtyEvent (EvKey key [MMeta])) =
  case key of
    KBS -> continue $ applyBackspaceWord s
    _   -> continue s
handleEvent s (VtyEvent (EvKey key []))
  | hasEnded s =
    case key of
      KEnter -> halt s
      KEsc   -> halt $ s {loop = True}
      _      -> continue s
  | otherwise =
    case key of
      KChar c -> handleChar c s
      KEnter  -> handleChar '\n' s
      KBS     -> continue $ applyBackspace s
      KEsc    -> halt $ s {loop = True}
      _       -> continue s
handleEvent s _ = continue s

app :: Attr -> Attr -> Attr -> App State e ()
app emptyAttr errorAttr resultAttr =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (emptyAttrName, emptyAttr)
          , (errorAttrName, errorAttr)
          , (resultAttrName, resultAttr)
          ]
    }

run :: Maybe Word8 -> Maybe Word8 -> String -> IO Bool
run fgEmptyCode fgErrorCode t = do
  s <- defaultMain (app emptyAttr errorAttr resultAttr) $ initialState t
  return $ loop s
  where
    emptyAttr = fg . ISOColor $ fromMaybe 8 fgEmptyCode
    errorAttr = flip withStyle bold . fg . ISOColor $ fromMaybe 1 fgErrorCode
    -- abusing the fgErrorCode to use as a highlight colour for the results here
    resultAttr = fg . ISOColor $ fromMaybe 1 fgErrorCode
