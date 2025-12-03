module AOC2023.Day1 where

import Data.Text qualified as T
import Utils

isNumber :: Char -> Bool
isNumber a = a `elem` ['0' .. '9']

-- Before plugin suggestion
-- filterNumbers list = filter isNumber list
filterNumbers :: [Char] -> [Char]
filterNumbers = filter isNumber

lineCalinbration :: String -> Int
lineCalinbration s =
  let numbers = filterNumbers s
      firstNum = head numbers
      lastNum = last numbers
   in read (firstNum : [lastNum]) :: Int

-- Curried/pre-applied function
processLines :: [String] -> [Int]
processLines = map lineCalinbration

-- insertNumbers :: String -> String
-- insertNumbers s =  s "eight"

part1 :: T.Text -> Int
part1 input = sum numbers
  where
    numbers = processLines $ lines $ T.unpack input

part2 :: T.Text -> Int
part2 input = sum numbers
  where
    numbers = processLines $ lines $ T.unpack input

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a