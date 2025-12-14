module AOC2025.Day10 where

import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.Parsec
  ( char,
    choice,
    digit,
    many,
    many1,
    newline,
    optional,
    space,
    try,
  )
import Text.Parsec.String (Parser)
import Utils qualified as U
import Data.Foldable (Foldable(toList))

type Button = [Int]

type Joltage = [Int]

data Light = On | Off deriving (Show, Eq)

data MachineInstruction = Instruction [Light] [Button] Joltage deriving (Show)

parseInstruction :: Parser [MachineInstruction]
parseInstruction =
  many1 $ Instruction <$> lightParser <*> buttonsParser <*> joltageParser <* newline
  where
    joltageParser = char '{' *> many1 (number <* optional (char ',')) <* char '}'
    buttonsParser = many $ char '(' *> many1 (number <* optional (char ',')) <* char ')' <* space
    lightParser = char '[' *> many1 (choice (try <$> [char '.' $> Off, char '#' $> On])) <* char ']' <* space

    number = read <$> many1 digit

part1 :: [String] -> Int
part1 i = sum r
  where
    input = U.runParser parseInstruction $ unlines i
    a = map (\(Instruction l b _) -> (l, List.subsequences b)) input
    r = map (\(l, bo) -> minimum $ map length $ filter (\b -> l == toggleLights l b) bo) a
    toggleLights :: [Light] -> [Button] -> [Light]
    toggleLights ls b = toList $ foldr toggles (Seq.fromList (replicate (length ls) Off)) $ concat b
      where
        toggles idx s = Seq.update idx newVal s
          where
            currentVal = fromMaybe Off $ Seq.lookup idx s
            newVal = case currentVal of On -> Off; Off -> On

part2 :: [String] -> Int
part2 i = length $ a
  where
    input = U.runParser parseInstruction $ unlines i
    a = map (\(Instruction _ b v) -> (v, b)) input

-- r =  map (\(l, bo)->  map length$ filter (\b -> l == increaseVolts l b) bo) a

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines