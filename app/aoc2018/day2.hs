module  AOC2018.Day2 where

import Data.List qualified as List
import Data.Map qualified as Map
import Utils qualified as U
import Data.Text qualified as T 

part1 :: [String] -> Int
part1 a = sum (twoOfAKind groupedLetters) * sum (threeOfAKind groupedLetters)
  where
    groupedLetters = groupBy <$> a
    meetsConditions s = if s > 0 then 1 else 0
    twoOfAKind m = meetsConditions . Map.size . Map.filter (== 2) <$> m
    threeOfAKind m = meetsConditions . Map.size . Map.filter (== 3) <$> m
    groupBy = foldl' byChar Map.empty
    byChar :: Map.Map Char Int -> Char -> Map.Map Char Int
    byChar m val = if Map.member val m then Map.insertWith (+) val 1 m else Map.insert val 1 m

part2 :: [String] -> String
part2 a = sameLetters result 
  where
    pairs = combinations 2 
    deltaOfOne p = (== length left - 1) . length . filter (uncurry (==)) $ charPairs
      where
        left =  head p
        right = List.last p
        charPairs = zip left right
    sameLetters x = right `List.intersect` left
      where
        left = head (head x)
        right = List.last (head x)
    result = filter deltaOfOne (pairs a)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs)
  | n > 0 = map (x :) (combinations (n - 1) xs) ++ combinations n xs
  | otherwise = [] -- Should not be reached due to n > 0 guard


part2' :: [String] -> String
part2' a =
  concat
    [ right `List.intersect` left
    | i <- [0 .. indexMax],
      let left = a !! i,
      j <- [i + 1 .. indexMax],
      let right = a !! j,
      deltaOfOne $ zip left right
    ]
  where
    indexMax = length a - 1
    deltaOfOne p = (== length p - 1) . length . filter (uncurry (==)) $ p

run :: T.Text -> U.Result
run a = U.Result'' result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines