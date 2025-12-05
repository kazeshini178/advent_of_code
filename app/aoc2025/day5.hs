module AOC2025.Day5 where

import Data.Text qualified as T
import Data.List qualified as List
import Data.List.Split
import Text.Parsec (char,  many1, digit)
import Text.Parsec.String (Parser)
import Utils qualified as U

type Range = (Int, Int)

parseInstruction :: Parser Range
parseInstruction = (,) <$> number <* char '-' <*> number
  where
    number = read <$> many1 digit

parseInt :: Parser Int
parseInt = number
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 a = length $ filter (\d -> any (\(s, e) -> s <= d && e >= d) ranges) ids
  where
    ranges = U.runParser parseInstruction <$> head lists
    ids = U.runParser parseInt <$> last lists
    lists = splitOn [""] a

part2 :: [String] -> Int
part2 a = fst $ foldl' (\(cc, l) c -> (determineTotal cc l c, determinePrev l c)) (0, (0, 0)) $ List.sortOn fst ranges
  where
    ranges = U.runParser parseInstruction <$> head lists
    lists = splitOn [""] a
    determineTotal c (ls, le) (s, e)
      | ls <= s && le >= e = c
      | s <= le && e > le = c + (e - le)
      | otherwise = c + ((e + 1) - s)
    determinePrev (ls, le) (s, e) = (if ls < s && s < le then ls else s, max e le)

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines