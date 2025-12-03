module AOC2015.Day3 (run) where

import Data.List (nub)
import qualified Data.Text  as T
import Utils

data Move = MoveUp | MoveDown | MoveLeft | MoveRight

data Coord = Coord Int Int deriving (Show, Eq, Ord)

parseInstructions :: Char -> Move
parseInstructions a = case a of
  '^' -> MoveUp
  'v' -> MoveDown
  '<' -> MoveLeft
  '>' -> MoveRight
  _ -> error "Invalid Direction"

createCoordList :: Move -> Coord -> [Coord] -> [Coord]
createCoordList move (Coord x y) list =
  case move of
    MoveUp -> Coord x (y + 1) : list
    MoveDown -> Coord x (y - 1) : list
    MoveLeft -> Coord (x - 1) y : list
    MoveRight -> Coord (x + 1) y : list

-- processMovements' :: [Move] -> [Coord] -> [Coord]
-- processMovements' [] a = a
-- processMovements' (x : xs) a = processMovements' xs newList
--   where
--     newList = createCoordList x (head a) a

processMovements :: [Move] -> [[Coord]] -> (Int -> Int) -> Int -> [Coord]
processMovements [] state _ _ = concat state
processMovements (x : xs) state switcher santa = processMovements xs nextState switcher nextSanta
  where
    santaState = state !! santa
    nextSanta = switcher santa
    newSantaState = createCoordList x (head santaState) santaState
    nextState = if santa == 0 then [newSantaState, state !! nextSanta] else [state !! nextSanta, newSantaState]

part1 :: T.Text -> Int
-- part1 a = length $ nub $ processMovements' movements [Coord 0 0]
part1 a = length $ nub $ processMovements movements [[Coord 0 0]] id 0
  where
    movements = parseInstructions <$> T.unpack a 

part2 :: T.Text -> Int
part2 a = length $ nub $ processMovements movements [[Coord 0 0], [Coord 0 0]] toggle 0
  where
    movements = parseInstructions <$> T.unpack a
    toggle i = if i == 1 then 0 else 1

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a