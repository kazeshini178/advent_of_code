module AOC2025.Day2 where

import Data.List.Split (chunksOf, splitOn)
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Parsec (char, digit, many1)
import Text.Parsec.String (Parser)
import Utils qualified as U

parseInstruction :: Parser (Int, Int)
parseInstruction = do
  start <- number <* char '-'
  end <- number
  pure (start, end)
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 a =
  sum $
    concatMap
      ( \(start, end) ->
          [ x
          | x <- [start .. end],
            let l = length (show x),
            even l,
            let s = Set.fromList $ chunksOf (l `div` 2) (show x),
            length s == 1
          ]
      )
      instructions
  where
    input = splitOn "," $ head a
    instructions = U.runParser parseInstruction <$> input

part2 :: [String] -> Int
part2 a =
  sum $ concatMap (\(start, end) -> filter isDupSequence [start .. end]) instructions
  where
    input = splitOn "," $ head a
    instructions = U.runParser parseInstruction <$> input
    isDupSequence v = not $ null dupSeq
      where
        str = show v
        maxSeq = length str `div` 2
        dupSeq =
          filter (\x -> allMatch $ chunksOf x str) $ [x | x <- [1 .. maxSeq], length str `mod` x == 0]
          where
            allMatch :: [String] -> Bool
            allMatch x = check (drop 1 x) (head x) True
            check [] _ r = r
            check (x : xs) val r
              | not r = False
              | otherwise = check xs x nr
              where
                nr = r && (val == x)

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines