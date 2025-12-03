module AOC2015.Day1 (run) where

import qualified  Data.Text  as T
import Utils

parseInstructions :: Char -> Int
parseInstructions a = case a of
  '(' -> 1
  ')' -> -1
  _ -> error "Invalid Direction"

stopWhen :: (Int -> Bool) -> Int -> Int -> [Int] -> Int
stopWhen _ pos _ [] = pos
stopWhen func pos acc (x : xs) =
  if func acc
    then pos
    else stopWhen func (pos + 1) (acc + x) xs

directions :: (Functor f) => f Char -> f Int
directions a = parseInstructions <$> a

part1 :: T.Text -> Int
part1 a = sum $ parseInstructions <$> T.unpack a

part2 :: T.Text -> Int
part2 a = stopWhen (== -1) 0 0 (directions (T.unpack a))

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a