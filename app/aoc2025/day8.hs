module AOC2025.Day8 where

import Data.Function (on)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text qualified as T
import Text.Parsec (char, digit, many, many1, newline, optional)
import Text.Parsec.String (Parser)
import Utils qualified as U

data Coord = Coord3d Int Int Int deriving (Show, Eq, Ord)

parseInstruction :: Parser [Coord]
parseInstruction =
  many $ Coord3d <$> number <* char ',' <*> number <* char ',' <*> number <* optional newline
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 i = foldl' (*) 1 $ take 3 $ List.sortBy (compare `on` Down) $ map length $ foldl' (\p (c1, c2) -> makeConnections p c1 c2) [] $ take 1000 $ map fst distanctPairs
  where
    input = U.runParser parseInstruction $ unlines i
    distanctPairs =
      List.sortBy (comparing snd) $
        [ ((c1, c2), distance)
        | a <- [0 .. length input - 2],
          let c1 = input !! a,
          b <- [a + 1 .. length input - 1],
          let c2 = input !! b,
          let distance = calcDistance c1 c2
        ]
    calcDistance (Coord3d x1 y1 z1) (Coord3d x2 y2 z2) = sqrt $ fromIntegral (x + y + z)
      where
        x = (x1 - x2) * (x1 - x2)
        y = (y1 - y2) * (y1 - y2)
        z = (z1 - z2) * (z1 - z2)
    makeConnections cons c1 c2 = newCircuits
      where
        circuit = [c1, c2]
        matching = concat $ filter (any (\c -> c == c1 || c == c2)) cons
        remaining = filter (not . any (\c -> c == c1 || c == c2)) cons
        newCircuits = List.nub (circuit ++ matching) : remaining

part2 :: [String] -> Int
part2 i = complete $ snd $ foldl' (\p (c1, c2) -> makeConnections p c1 c2) ([], (Coord3d 0 0 0, Coord3d 0 0 0)) $ map fst distanctPairs
  where
    complete (Coord3d x1 _ _, Coord3d x2 _ _) = x1 * x2
    input = U.runParser parseInstruction $ unlines i
    distanctPairs =
      List.sortBy (comparing snd) $
        [ ((c1, c2), distance)
        | a <- [0 .. length input - 2],
          let c1 = input !! a,
          b <- [a + 1 .. length input - 1],
          let c2 = input !! b,
          let distance = calcDistance c1 c2
        ]
    calcDistance (Coord3d x1 y1 z1) (Coord3d x2 y2 z2) = sqrt $ fromIntegral (x + y + z)
      where
        x = (x1 - x2) * (x1 - x2)
        y = (y1 - y2) * (y1 - y2)
        z = (z1 - z2) * (z1 - z2)
    makeConnections (cons, pc) c1 c2 = if c1 `elem` first && c2 `elem` first then (cons, pc) else (newCircuits, prevConnection)
      where
        circuit = [c1, c2]
        (matching, remaining) = List.partition (\c -> c1 `elem` c || c2 `elem` c) cons
        newCircuits = List.nub (circuit ++ concat matching) : remaining
        (first, _) = fromMaybe ([], [[]]) $ List.uncons cons
        (nfirst, _) = fromMaybe ([], [[]]) $ List.uncons newCircuits
        prevConnection = if c1 `elem` first && c2 `elem` first && length nfirst == length first then pc else (c1, c2)

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines