module AOC2025.Module (runChallenge) where

import AOC2025.Day1 qualified as Day1
import AOC2025.Day2 qualified as Day2
import AOC2025.Day3 qualified as Day3
import Data.Text qualified as T
import Utils ( getChallengeData, Result)
import Data.Time.Clock qualified as Clock
import Data.Time (NominalDiffTime)
import Data.Fixed ( E12, Fixed )
import Text.Printf (printf)
import GHC.IO (evaluate)

getModuleChallengeData :: Int -> IO T.Text
getModuleChallengeData = getChallengeData 2025

execute :: (Eq a, Num a) => a -> (T.Text -> Result)
execute a = case a of
  1 -> Day1.run
  2 -> Day2.run
  3 -> Day3.run
  4 -> error "Not Implemented"
  5 -> error "Not Implemented"
  6 -> error "Not Implemented"
  7 -> error "Not Implemented"
  8 -> error "Not Implemented"
  9 -> error "Not Implemented"
  10 -> error "Not Implemented"
  11 -> error "Not Implemented"
  12 -> error "Not Implemented"
  _ -> error "Invalid Day, AOC only runns from the 1st to the 12th since 2025"

runChallenge :: Int -> IO Result
runChallenge day = do
  content <- getModuleChallengeData day
  start <- Clock.getCurrentTime
  result <- evaluate $ execute day content
  end <- Clock.getCurrentTime

  print (formatNominalDiffTime $ Clock.diffUTCTime end start)
  pure result


formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime diff
  | diff == 0 = "-"
  | diff < ps = roundFixed (realToFrac $ diff / fs) <> "fs"
  | diff < ns = roundFixed (realToFrac $ diff / ps) <> "ps"
  | diff < us = roundFixed (realToFrac $ diff / ns) <> "ns"
  | diff < ms = roundFixed (realToFrac $ diff / us) <> "us"
  | diff < s = roundFixed (realToFrac $ diff / ms) <> "ms"
  | diff < m = roundFixed (realToFrac $ diff / s) <> "s"
  | otherwise = roundFixed (realToFrac $ diff / m) <> "m"
  where
    roundFixed :: Fixed E12 -> String
    roundFixed f = printf "%.2f" (realToFrac f :: Double)
    fs = 1e-15
    ps = 1e-12
    ns = 1e-9
    us = 1e-6
    ms = 1e-3
    s = 1e0
    m = 60e0