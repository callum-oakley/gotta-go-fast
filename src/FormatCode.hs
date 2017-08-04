module FormatCode (toAscii, trimEmptyLines) where

import Data.Char (isAscii, isPrint)

toAscii :: Int -> String -> String
toAscii tabWidth = concatMap toAscii'
  where
    toAscii' c
      | c == '\t' = replicate tabWidth ' '
      | c == '‘' || c == '’' = "'"
      | c == '“' || c == '”' = "\""
      | c == '–' || c == '—' = "-"
      | c == '…' = "..."
      | isAscii c && (isPrint c || c == '\n') = [c]
      | otherwise = ""

trimEmptyLines :: String -> String
trimEmptyLines = (++ "\n") . f . f
  where
    f = reverse . dropWhile (== '\n')

getIndent :: String -> String
getIndent = takeWhile (== ' ')

ensureEndWithNewline :: String -> String
ensureEndWithNewline "" = "\n"
ensureEndWithNewline s
  | last s == '\n' = s
  | otherwise = s ++ "\n"