module AOC2025.Day4 where

import Data.Functor
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec (char, choice, many, try)
import Text.Parsec.String (Parser)
import Utils qualified as U

parseInstruction :: Parser [Block]
parseInstruction = many $ choice $ try <$> [char '.' $> Space, char '@' $> Roll]

data Block = Roll | Space | X deriving (Eq)

-- For Dedugging
instance Show Block where
  show Roll = "@"
  show Space = "."
  show X = "x"

part1 :: [String] -> Int
part1 a = length accessible
  where
    gridSize = length a
    grid = V.fromList $ U.runParser parseInstruction $ concat a
    accessible = V.ifilter (\idx b -> case b of Roll -> surroundingRolls gridSize idx < 4; _ -> False) grid
    surroundingRolls gs p = length $ filter (== Roll) [topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight]
      where
        topLeft = if ((p - 1) - gs) `mod` gs == gs - 1 || (p - 1) - gs <= 0 then Space else (V.!) grid ((p - 1) - gs)
        top = if p - gs <= 0 then Space else (V.!) grid (p - gs)
        topRight = if ((p + 1) - gs) `mod` gs == 0 || (p + 1) - gs <= 0 then Space else (V.!) grid ((p + 1) - gs)
        left = if p `mod` gs == 0 then Space else (V.!) grid (p - 1)
        right = if p `mod` gs == gs - 1 then Space else (V.!) grid (p + 1)
        bottomLeft = if (p - 1 + gs) `mod` gs == gs - 1 || (p - 1 + gs) >= length grid then Space else (V.!) grid (p - 1 + gs)
        bottom = if (p + gs) >= length grid then Space else (V.!) grid (p + gs)
        bottomRight = if (p + 1 + gs) `mod` gs == 0 || (p + 1 + gs) >= length grid then Space else (V.!) grid (p + 1 + gs)

part2 :: [String] -> Int
part2 a = totalRemoved grid 0
  where
    gridSize = length a
    grid = V.fromList $ U.runParser parseInstruction $ concat a
    totalRemoved g c = if V.null accessible then c else totalRemoved matched (c + length accessible)
      where
        accessible = V.filter (\(idx, b) -> case b of Roll -> surroundingRolls g gridSize idx < 4; _ -> False) $ V.indexed g
        matched = V.map (\s -> if s `V.elem` accessible then X else snd s) $ V.indexed g
    surroundingRolls g gs p = length $ filter (== Roll) [topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight]
      where
        topLeft = if ((p - 1) - gs) `mod` gs == gs - 1 || (p - 1) - gs <= 0 then Space else (V.!) g ((p - 1) - gs)
        top = if p - gs <= 0 then Space else (V.!) g (p - gs)
        topRight = if ((p + 1) - gs) `mod` gs == 0 || (p + 1) - gs <= 0 then Space else (V.!) g ((p + 1) - gs)
        left = if p `mod` gs == 0 then Space else (V.!) g (p - 1)
        right = if p `mod` gs == gs - 1 then Space else (V.!) g (p + 1)
        bottomLeft = if (p - 1 + gs) `mod` gs == gs - 1 || (p - 1 + gs) >= length g then Space else (V.!) g (p - 1 + gs)
        bottom = if (p + gs) >= length g then Space else (V.!) g (p + gs)
        bottomRight = if (p + 1 + gs) `mod` gs == 0 || (p + 1 + gs) >= length g then Space else (V.!) g (p + 1 + gs)

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines