module GottaGoFast where

import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Time (UTCTime)

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data RelativeLocation = BeforeCursor | AfterCursor

data State = State
  { target  :: String
  , input   :: String
  , start   :: Maybe UTCTime
  , end     :: Maybe UTCTime
  , strokes :: Integer
  , hits    :: Integer
  }

hasEnded :: State -> Bool
hasEnded = isJust . end

hasStarted :: State -> Bool
hasStarted = isJust . start

cursorCol :: State -> Int
cursorCol = length . takeWhile (/= '\n') . reverse . input

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input

cursor :: State -> (Int, Int)
cursor s = (cursorCol s, cursorRow s)

isHit :: State -> Char -> Bool
isHit s c = (input s ++ [c]) `isPrefixOf` target s

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
applyBackspace s = s {input = backspace $ input s}

backspaceWord :: String -> String
backspaceWord xs = reverse $ dropWhile (/= ' ') $ reverse $ backspace xs

applyBackspaceWord :: State -> State
applyBackspaceWord s = s { input = backspaceWord $ input s }

initialState :: String -> State
initialState t = State
  { target = t
  , input = ""
  , start = Nothing
  , end = Nothing
  , strokes = 0
  , hits = 0
  }
