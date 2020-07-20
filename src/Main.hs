{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Control.Monad          (filterM)
import           Data.Char              (isAscii, isPrint)
import           Data.FileEmbed         (embedStringFile)
import           Data.List.Split        (splitOn)
import qualified Data.Text              as T
import           Data.Word              (Word8)
import           System.Console.CmdArgs (Data, Typeable, args, cmdArgs, def,
                                         help, program, summary, typ, (&=))
import           System.Directory       (doesFileExist)
import           System.Random          (randomRIO)
import           Text.Wrap              (WrapSettings (..), wrapText)

import           UI                     (run)

-- TODO expose these as options
minParagraphLen :: Int
minParagraphLen = 200

maxParagraphLen :: Int
maxParagraphLen = 800

-- This makes for a 60 second test at 100 WPM
nonsenseLen :: Int
nonsenseLen = 500

data Config =
  Config
    { height    :: Int
    , width     :: Int
    , tab       :: Int
    , files     :: [FilePath]
    , fg_empty  :: Maybe Word8
    , fg_error  :: Maybe Word8
    , paragraph :: Bool
    , reflow_   :: Bool
    }
  deriving (Show, Data, Typeable)

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

config :: Config
config =
  Config
    { paragraph = def &= help "Sample a single paragraph from the input files"
    , reflow_ = def &= help "Reflow paragraph to the target width"
    , height =
        20 &= typ "LINES" &=
        help "The maximum number of lines to sample (default: 20)"
    , width =
        80 &= typ "CHARS" &=
        help "The width at which to wrap lines (default: 80)"
    , tab = 4 &= typ "SIZE" &= help "The size of a tab in spaces (default: 4)"
    , fg_empty =
        def &= typ "COLOUR" &=
        help "The ISO colour code for empty (not yet typed) characters"
    , fg_error = def &= typ "COLOUR" &= help "The ISO colour code for errors"
    , files = def &= args &= typ "FILES"
    } &=
  summary "Gotta Go Fast 0.2.1.1" &=
  help "Practice typing and measure your WPM and accuracy" &=
  program "gotta-go-fast"

wrap :: Int -> String -> String
wrap width = T.unpack . wrapText wrapSettings width . T.pack

wrapSettings = WrapSettings {preserveIndentation = True, breakLongWords = True}

-- wordWeights.txt is taken from
-- https://en.wiktionary.org/wiki/Wiktionary:Frequency_lists#TV_and_movie_scripts
-- (and cleaned up a little with some throwaway sed)
wordWeights :: [(String, Int)]
wordWeights =
  map ((\[w, f] -> (w, read f)) . words) . lines $
  $(embedStringFile "wordWeights.txt")

totalWeight :: Int
totalWeight = sum . map snd $ wordWeights

weightedRandomWord :: IO String
weightedRandomWord = do
  r <- randomRIO (0, totalWeight - 1)
  return $ go r wordWeights
  where
    go r ((w, f):rest)
      | r < f = w
      | otherwise = go (r - f) rest

-- Generates nonsense which is superficially similar to English. Similar in the
-- sense that the frequency of words in the generated text is approximately the
-- same as the frequency of words in actual usage.
nonsense :: Config -> IO String
nonsense c = do
  words <- go nonsenseLen
  return $ (wrap (width c) . unwords $ words) ++ "\n"
  where
    go :: Int -> IO [String]
    go n
      | n <= 0 = return []
      | otherwise = do
        word <- weightedRandomWord
        rest <- go (n - length word)
        return $ word : rest

sample :: Config -> String -> IO String
sample c file =
  if paragraph c
    then sampleParagraph
    else sampleLines
  where
    sampleParagraph = do
      r <- randomRIO (0, length paragraphs - 1)
      return .
        (if reflow_ c
           then reflow
           else id) $
        paragraphs !! r
    sampleLines = do
      r <- randomRIO (0, max 0 $ length (lines ascii) - height c)
      return . trimEmptyLines . chop . wrap (width c) . chop . unlines . drop r $
        lines ascii
    paragraphs =
      filter ((\l -> l >= minParagraphLen && l <= maxParagraphLen) . length) .
      map unlines . splitOn [""] . lines $
      ascii
    reflow s =
      (wrap (width c) .
       map
         (\c ->
            if c == '\n'
              then ' '
              else c) $
       s) ++
      "\n"
    ascii = toAscii (tab c) file
    chop = unlines . take (height c) . lines

main :: IO ()
main = do
  c <- cmdArgs config
  fs <- filterM doesFileExist $ files c
  case fs of
    [] -> nonsense c >>= run (fg_empty c) (fg_error c)
    _ -> do
      r <- randomRIO (0, length fs - 1)
      file <- readFile $ fs !! r
      sampled <- sample c file
      run (fg_empty c) (fg_error c) sampled
