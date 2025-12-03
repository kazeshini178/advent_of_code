module AOC2018.Module (runChallenge) where

import AOC2018.Day1 qualified as Day1
import AOC2018.Day2 qualified as Day2
import AOC2018.Day3 qualified as Day3
import AOC2018.Day4 qualified as Day4
import AOC2018.Day5 qualified as Day5
import AOC2018.Day7 qualified as Day7
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Formatting
import Formatting.Clock (timeSpecs) 
import System.Clock 
import Utils

getModuleChallengeData :: Int -> IO T.Text
getModuleChallengeData = getChallengeData 2018

execute :: (Eq a, Num a) => a -> (T.Text -> Result)
execute a = case a of
  1 -> Day1.run
  2 -> Day2.run
  3 -> Day3.run
  4 -> Day4.run
  5 -> Day5.run
  6 -> error "Not Implemented"
  7 -> Day7.run
  8 -> error "Not Implemented"
  9 -> error "Not Implemented"
  10 -> error "Not Implemented"
  11 -> error "Not Implemented"
  12 -> error "Not Implemented"
  13 -> error "Not Implemented"
  14 -> error "Not Implemented"
  15 -> error "Not Implemented"
  16 -> error "Not Implemented"
  17 -> error "Not Implemented"
  18 -> error "Not Implemented"
  19 -> error "Not Implemented"
  20 -> error "Not Implemented"
  21 -> error "Not Implemented"
  22 -> error "Not Implemented"
  23 -> error "Not Implemented"
  24 -> error "Not Implemented"
  25 -> error "Not Implemented"
  _ -> error "Invalid Day, AOC only runns from the 1st to the 25th"

runChallenge :: Int -> IO Result
runChallenge day = do
  content <- getModuleChallengeData day
  start <- liftIO $ getTime Monotonic
  let result = execute day content
  end <- liftIO $ getTime Monotonic

  formatedDuration <- fprint timeSpecs start end
  pure result