module AOC2024.Day1 where

import Data.Text qualified as T
import Utils
import Text.Parsec (parse, digit, many1, many, space)
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.List (sort)
 
data LocationSet = Set Int Int | None

parseIds ::  Parser  LocationSet
parseIds = Set <$> number <* many space <*> number
  where
    number = read <$> many1 digit

part1 :: T.Text -> Int
part1 input = -1 
  where
    -- arrays = $ parsedLocations
    parsedLocations = fromRight None . parse parseIds "" <$> lineInput
    lineInput = lines $ T.unpack input

part2 :: T.Text -> Int
part2 input =  -1

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a