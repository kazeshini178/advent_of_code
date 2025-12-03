module AOC2018.Day1 (run) where

import Text.Parsec
import Text.Parsec.String
import Data.Set
import Utils qualified as U
import Data.Text qualified as T

parseNumber :: Parser Int
parseNumber = do
  symbol <- choice $ try <$> [char '+', char '-']
  val <- read <$> many1 digit
  return $ if symbol == '+' then val else -val

part1 :: T.Text -> Int
part1 a = sum nums
  where
    inputLines = lines $ T.unpack a
    nums = U.runParser parseNumber <$> inputLines

part2 :: T.Text -> Int
part2 a = findRepeatedFrequency nums 0 empty False 0
  where
    inputLines = lines $ T.unpack a
    nums = U.runParser parseNumber <$> inputLines
    findRepeatedFrequency _ _ _ True prev = prev
    findRepeatedFrequency list idx stateSet False prev = findRepeatedFrequency list nextIdx (insert freq stateSet) freqRepeated freq
      where
        nextIdx = if idx == length list - 1 then 0 else idx + 1 
        freq = prev + list !! idx
        freqRepeated = freq `member` stateSet


part2' :: [String] -> Int
part2' a = repeatedFreq nums 0 empty 0
  where
    nums = U.runParser parseNumber <$> a
    repeatedFreq list idx stateSet prev =
      if freqRepeated
          then freq
          else repeatedFreq list nextIdx (insert freq stateSet) freq
      where
        len = length list
        nextIdx = if idx == len - 1 then 0 else idx + 1
        item = list !! idx
        freq = prev + item
        freqRepeated = freq `member` stateSet

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a