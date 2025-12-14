module AOC2025.Day6 where

import Data.List qualified as List
import Data.List.Extra (trim)
import Data.List.Split ( splitOn )
import Data.Text qualified as T
import Text.Parsec( char, digit, space, choice, optional, many, many1, try )
import Text.Parsec.String (Parser)
import Utils qualified as U
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

data Input = Value Int | Symbol Char deriving (Show)

parseInstruction :: Parser [Input]
parseInstruction =
  many $
    choice $
      try
        <$> [ Value <$> (many space *> number <* many space),
              optional (many space) *> (Symbol <$> choice (try <$> [char '+',char '*']))
            ]
  where
    number = read <$> many1 digit


action :: Char -> [Int] -> Int
action '+' vals = sum vals
action '*' vals = product vals

part1 :: [String] -> Int
part1 a = trace (show $ map List.unsnoc $List.transpose input)$ sum . map (sumUp . fromMaybe ([], Symbol ' ') . List.unsnoc) $ List.transpose input
  where
    input = U.runParser parseInstruction <$>  a
    sumUp (values, Symbol symbol) = action symbol . map (\(Value x) -> x) $ values

part2 :: [String] -> Int
part2 = sum . map sumUp . splitOn [""].  map trim . reverse . List.transpose
  where
    -- r =  map trim . reverse . map (Data.Bifunctor.first List.transpose) $ fromMaybe  [([],"")] $ List.unsnoc a
    sumUp :: [String] -> Int
    sumUp s = action symbol $ map (read . trim .  takeWhile (/= symbol)) s
      where
        symbol = last $ last s

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines