module AOC2025.Day7 where

import Data.Functor (($>))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec (char, choice, many, try)
import Text.Parsec.String (Parser)
import Utils qualified as U

data Input = Start | Free | Splitter deriving (Eq)

parseInstruction :: Parser [Input]
parseInstruction =
  many $ choice $ try <$> [char '.' $> Free, char 'S' $> Start, char '^' $> Splitter]

part1 :: [String] -> Int
part1 a = tachyons
  where
    input = U.runParser parseInstruction <$> a
    startIndex = fromMaybe 0 $ List.elemIndex Start $ head input
    tachyons = fst $ foldl' (\(c, ps) i -> countSplits ps i c) (0, [startIndex]) $ drop 1 input
    countSplits p r c = (c + newCount, newP)
      where
        splitters = map (\cp -> if r !! cp == Splitter then (1, [cp - 1, cp + 1]) else (0, [cp])) p
        newP = List.nub $ concatMap snd splitters
        newCount = foldl' (+) 0 $ map fst splitters

part2 :: [String] -> Int
part2 a = countTimelines input [startIndex] (Map.fromList [(startIndex, 1)])
  where
    input = V.fromList . U.runParser parseInstruction <$> a
    startIndex = fromMaybe 0 $ V.elemIndex Start $ head input
    countTimelines [] _ cache = sum $ Map.elems cache
    countTimelines (x : xs) idx cache = if allFree then countTimelines xs idx cache else countTimelines xs newPaths ncache
      where
        allFree = all (== Free) x
        ncache = foldl' updateCache cache idx
        newPaths = Map.keys $ Map.filter (/= 0) ncache
        updateCache p c =
          if x V.! c == Splitter
            then
              let val = fromMaybe 1 $ Map.lookup c p
               in Map.insertWith (+) (c + 1) val $ Map.insertWith (+) (c - 1) val $ Map.insert c 0 p
            else p

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines