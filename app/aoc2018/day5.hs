module  AOC2018.Day5 where

import Data.List qualified as List
import Data.Char (toLower, isUpper, isLower)
import Utils qualified as U
import Data.Text qualified as T

part1 :: [String] -> Int
part1 a = length $ triggerUnits (head a) []

triggerUnits :: String -> String -> String
triggerUnits [] r = filter (/= '\0') r
triggerUnits [f] r = r ++ [f]
triggerUnits (f : s : xs) r =
  if canDestroy f s
    then triggerUnits remaining newString
    else triggerUnits (s : xs) (r ++ [f])
  where
    remaining = if null r then xs else List.last r : xs
    newString = if null r then [] else init r
    canDestroy x y = toLower x == toLower y && ((isUpper x && isLower y) || (isLower x && isUpper y))

part2 :: [String] -> Int
part2 a = List.minimum $ res alpha
  where
    val = head a
    alpha = ['a'..'z']
    res = map (length .(`triggerUnits` []) . (\s -> filter (\l -> toLower l /= s) val)) --`using` parList rseq


run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines