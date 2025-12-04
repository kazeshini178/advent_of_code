module AOC2025.Day1 where

import Data.Text qualified as T
import Text.Parsec (char, choice, digit, many1, try)
import Text.Parsec.String (Parser)
import Utils

parseInstruction :: Parser Int
parseInstruction = do
  direction <- choice $ try <$> [char 'L', char 'R']
  num <- number
  pure $ if direction == 'L' then -num else num
  where
    number = read <$> many1 digit

part1 :: T.Text -> Int
part1 a = countZeros instructions 50 0
  where
    lineInput = lines $ T.unpack a
    instructions = runParser parseInstruction <$> lineInput
    countZeros [] _ c = c
    countZeros (x : xs) p c = countZeros xs nextPosition zeroCount
      where
        nextPosition = (p + x) `mod` 100
        zeroCount = if nextPosition == 0 then c + 1 else c

part2 :: T.Text -> Int
part2 a =
  countZeros instructions 50 0
  where
    lineInput = lines $ T.unpack a
    instructions = runParser parseInstruction <$> lineInput
    countZeros [] _ c = c
    countZeros (x : xs) p c =
      countZeros xs nextPosition (zeroCount + fullRotations + zerosPassed)
      where
        nextPosition = (p + x) `mod` 100
        zeroCount = if nextPosition == 0 then c + 1 else c
        fullRotations = abs x `div` 100
        zerosPassed = if nextPosition /= 0 && p /= 0 && ((x > 0 && nextPosition < p) || (x < 0 && nextPosition > p)) then 1 else 0

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a