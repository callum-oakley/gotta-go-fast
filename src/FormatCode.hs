module FormatCode (expandTabs, trimEmptyLines, wordWrap) where

expandTabs :: Int -> String -> String
expandTabs n = concatMap (\c -> if c == '\t' then replicate n ' ' else [c])

trimEmptyLines :: [String] -> [String]
trimEmptyLines = reverse . dropWhile (== "") . reverse . dropWhile (== "")

getIndent :: String -> String
getIndent = takeWhile (== ' ')

ensureEndWithNewline :: String -> String
ensureEndWithNewline "" = "\n"
ensureEndWithNewline s
  | last s == '\n' = s
  | otherwise = s ++ "\n"

-- TODO Replace this atrocity with a library function (which is currently
-- buggy, need to submit a PR).
wordWrap :: Int -> String -> String
wordWrap n = result . foldl greedy ("", "", "", "") . ensureEndWithNewline
  where
    result (acc, _, _, _) = acc
    greedy (acc, l, s, w) '\n'
      | length (l ++ s ++ w) <= n = (acc ++ l ++ s ++ w ++ "\n", "", "", "")
      | otherwise = (acc ++ l ++ "\n" ++ getIndent l ++ w ++ "\n", "", "", "")
    greedy (acc, l, s, "") ' ' = (acc, l, s ++ " ", "")
    greedy (acc, l, s, w) ' '
      | length (l ++ s ++ w) <= n = (acc, l ++ s ++ w, " ", "")
      | otherwise = (acc ++ l ++ "\n", getIndent l ++ w, " ", "")
    greedy (acc, l, s, w) c
      | length (getIndent l ++ w) < n = (acc, l, s, w ++ [c])
      | otherwise =
        (acc ++ l ++ "\n" ++ getIndent l ++ w ++ "\n", getIndent l, "", [c])
