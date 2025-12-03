module  AOC2018.Day7 where

import Text.Parsec
import Text.Parsec.String
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Set qualified as Set
import Utils qualified as U

data Instruction = Step Char Char

parseInstructions :: Parser Instruction
parseInstructions = do
  before <- string "Step " *> anyChar
  after <- string " must be finished before step " *> anyChar <* string " can begin."
  pure $ Step before after

-- Start with B end on C
part1 :: [String] -> String
part1 a = instructionOrder startingSteps []
  where
    mapSteps = Map.map List.sort . foldl' (\p (Step bef aft) -> Map.insertWith (++) bef [aft] p) Map.empty $ U.runParser parseInstructions <$> a
    startingSteps = Map.keys . Map.filterWithKey (\k _ -> k `Set.notMember` uniqueNextSteps) $ mapSteps
      where
        uniqueNextSteps = Map.foldl' (\p c -> p `Set.union` Set.fromList c) Set.empty mapSteps
    instructionOrder :: [Char] -> [Char] -> [Char]
    instructionOrder [] r = List.reverse r
    instructionOrder (x : xs) r = instructionOrder (List.sort (nextSteps ++ xs)) order
      where
        order = if x `elem` r || not prerequistesComplete then r else x : r
        nextSteps = List.sort . fromMaybe "" $ Map.lookup x mapSteps
        prerequistesComplete = Map.null . Map.filterWithKey (\k s -> k `notElem` r && x `elem` s) $ mapSteps

part2 :: [String] -> Int
part2 a = 0
-- TODO: Brain no worky
-- part2 :: [String] -> Int
-- part2 a = allocateWork [('\0', 0 :: Int),('\0', 0),('\0', 0),('\0', 0),('\0', 0)] startingSteps [] + 1560 -- Const Seconds per step
--   where
--     mapSteps = Map.map List.sort . foldl' (\p (Step bef aft) -> Map.insertWith (++) bef [aft] p) Map.empty $ haskellParseBsHider parseInstructions <$> a
--     startingSteps = Map.keys . Map.filterWithKey (\k _ -> k `Set.notMember` uniqueNextSteps) $ mapSteps
--       where
--         uniqueNextSteps = Map.foldl' (\p c -> p `Set.union` Set.fromList c) Set.empty mapSteps
--     allocateWork w l r = 0
--       where
--         nw = List.map (\cw -> if isFree cw then allocate else cw ) w
--         isFree (l, t) = l == '\0' || t == 0
--         allocate = 

run :: T.Text -> U.Result
run a = U.Result' result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines