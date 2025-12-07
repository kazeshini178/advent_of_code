module AOC2025.Day6 where

import Data.Functor (($>))
import Data.List qualified as List
import Data.List.Extra (trim)
import Data.List.Split ( splitOn )
import Data.Text qualified as T
import Text.Parsec( char, digit, space, choice, optional, many, many1, try )
import Text.Parsec.String (Parser)
import Utils qualified as U

data Input = Value Int | Plus | Multi deriving (Show)

parseInstruction :: Parser [Input]
parseInstruction =
  many $
    choice $
      try
        <$> [ Value <$> (many space *> number <* many space),
              optional (many space) *> (char '+' $> Plus),
              optional (many space) *> (char '*' $> Multi)
            ]
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 a = sum $ map sumUp $ List.transpose input
  where
    input = U.runParser parseInstruction <$> a
    sumUp s = action symbol $ map (\(Value x) -> x) values
      where
        action Plus vals = foldl' (+) 0 vals
        action Multi vals = foldl' (*) 1 vals

        values = take (length s - 1) s
        symbol = head $ take 1 (drop (length s - 1) s)

part2 :: [String] -> Int
part2 a = sum $ map sumUp $ splitOn [""] $ map trim $ reverse $ List.transpose a
  where
    sumUp s = ints
      where
        stringToInt s = read s :: Int
        ints =action symbol $ map (stringToInt . trim .  takeWhile (/= symbol)) s
        action :: Char -> [Int] -> Int
        action '+' vals = foldl' (+) 0 vals
        action '*' vals = foldl' (*) 1 vals
        symbol = last $ last s

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines