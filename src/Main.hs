{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Control.Monad          (filterM, when)
import           Data.Char              (isAscii, isPrint)
import           Data.FileEmbed         (embedStringFile)
import           Data.List.Split        (splitOn)
import qualified Data.Text              as T
import           Data.Word              (Word8)
import           System.Console.CmdArgs (Data, Typeable, args, cmdArgs, def,
                                         details, help, name, program, summary,
                                         typ, (&=))
import           System.Directory       (doesFileExist)
import           System.Random          (randomRIO)
import           Text.Wrap              (WrapSettings (..), wrapText)

import           UI                     (run)

data Config =
  Config
    { fg_empty          :: Maybe Word8
    , fg_error          :: Maybe Word8
    , files             :: [FilePath]
    , height            :: Int
    , max_paragraph_len :: Int
    , min_paragraph_len :: Int
    , nonsense_len      :: Int
    , paragraph         :: Bool
    , reflow_           :: Bool
    , tab               :: Int
    , width             :: Int
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
    { fg_empty =
        def &= typ "COLOUR" &=
        help "The ANSI colour code for empty (not yet typed) text"
    , fg_error = def &= typ "COLOUR" &= help "The ANSI colour code for errors"
    , height =
        20 &= typ "LINES" &=
        help "The maximum number of lines to sample (default: 20)"
    , max_paragraph_len =
        750 &= typ "WORDS" &=
        help "The maximum length of a sampled paragraph (default: 750)"
    , min_paragraph_len =
        250 &= typ "WORDS" &=
        help "The minimum length of a sampled paragraph (default: 250)"
    , nonsense_len =
        500 &= name "l" &= typ "WORDS" &=
        help "The length of nonsense to generate (default: 500)"
    , paragraph = def &= help "Sample a paragraph from the input files"
    , reflow_ = def &= help "Reflow paragraph to the target width"
    , tab = 4 &= typ "SIZE" &= help "The size of a tab in spaces (default: 4)"
    , width =
        80 &= typ "CHARS" &=
        help "The width at which to wrap lines (default: 80)"
    , files = def &= args &= typ "FILES"
    } &=
  summary "Gotta Go Fast 0.3.0.3" &=
  help "Practice typing and measure your WPM and accuracy." &=
  program "gotta-go-fast" &=
  details (lines $(embedStringFile "details.txt"))

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

loopWhile :: Monad m => (a -> Bool) -> m a -> m a
loopWhile p mx = do
  x <- mx
  if p x
    then loopWhile p mx
    else return x

-- Generates nonsense which is superficially similar to English. Similar in the
-- sense that the frequency of words in the generated text is approximately the
-- same as the frequency of words in actual usage.
nonsense :: Config -> IO String
nonsense c = do
  words <- go (nonsense_len c) Nothing
  return $ (wrap (width c) . unwords $ words) ++ "\n"
  where
    go n lastWord
      | n <= 0 = return []
      | otherwise = do
        word <- loopWhile ((== lastWord) . Just) weightedRandomWord
        rest <- go (n - length word - 1) (Just word) -- extra 1 to count the space
        return $ word : rest

sample :: Config -> String -> IO String
sample c file =
  if paragraph c && not (null paragraphs)
    then sampleParagraph
    else sampleLines
  where
    sampleParagraph = do
      r <- randomRIO (0, length paragraphs - 1)
      return $
        (if reflow_ c
           then reflow
           else wrap (width c))
          (paragraphs !! r) ++
        "\n"
    sampleLines = do
      r <- randomRIO (0, max 0 $ length (lines ascii) - height c)
      return . trimEmptyLines . chop . wrap (width c) . chop . unlines . drop r $
        lines ascii
    paragraphs =
      filter
        ((\l -> l >= min_paragraph_len c && l <= max_paragraph_len c) . length) .
      map unlines . splitOn [""] . lines $
      ascii
    reflow =
      wrap (width c) .
      map
        (\case
           '\n' -> ' '
           c -> c)
    ascii = toAscii (tab c) file
    chop = unlines . take (height c) . lines

main :: IO ()
main = do
  c <- cmdArgs config
  fs <- filterM doesFileExist $ files c
  loop <-
    case fs of
      [] -> nonsense c >>= run (fg_empty c) (fg_error c)
      _ -> do
        r <- randomRIO (0, length fs - 1)
        file <- readFile $ fs !! r
        sampled <- sample c file
        run (fg_empty c) (fg_error c) sampled
  when loop main
