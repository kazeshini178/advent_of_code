{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
module AOC2025.Day3 where

import Data.Char (digitToInt)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Utils qualified as U

part1 :: [String] -> Int
part1 a = sum $ map maxJoltage a

part2 :: [String] -> Int
part2 a = sum $ map maxJoltage' a

maxJoltage :: String -> Int
maxJoltage input = maximum $ [x | z <- [0 .. length input - 2], c <- [z + 1 .. length input - 1], z /= c, let x = read $ (input !! z) : [input !! c] :: Int]

maxJoltage' :: String -> Int
maxJoltage' input = read . concatMap show $ pickBatteries mappedInts (12 :: Int) [] []
  where
    mappedInts = zip [(1 :: Int) ..] $ map digitToInt input

pickBatteries :: [(Int, Int)] -> Int -> [Int] -> [Int] -> [Int]
pickBatteries _ 0 _ r = r
pickBatteries x tn ex r
  | i `elem` ex = r
  | length remaining >= tn - 1 = pickBatteries remaining (tn - 1) [] (r ++ [v])
  | otherwise = pickBatteries x tn (i : ex) r
  where
    (i, v) = maximumBy comparer x
    comparer :: (Int, Int) -> (Int, Int) -> Ordering
    comparer (fi, fv) (si, sv) =
      if firstIsGreater
        then if not isFirstExcluded then GT else if isSecondExcluded then EQ else LT
        else if not isSecondExcluded then LT else if isFirstExcluded then EQ else GT
      where
        firstIsGreater = fv >= sv
        isFirstExcluded = fi `elem` ex
        isSecondExcluded = si `elem` ex
    foudnIdx = fromMaybe 0 $ elemIndex (i, v) x
    remaining = drop (foudnIdx + 1) x

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines