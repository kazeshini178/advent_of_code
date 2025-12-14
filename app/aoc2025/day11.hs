module AOC2025.Day11 where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe,fromJust)
import Data.MemoTrie
import Data.Text qualified as T
import Text.Parsec
  ( letter,
    many,
    many1,
    newline,
    optional,
    string,
  )
import Text.Parsec.String (Parser)
import Utils qualified as U

parseInstruction :: Parser [(String, [String])]
parseInstruction =
  many1 $ (,) <$> (many1 letter <* string ": ") <*> many (many1 letter <* optional (string " ")) <* newline

part1 :: [String] -> Int
part1 i = countPaths (getValue "you" input) 0
  where
    input = Map.fromList $ U.runParser parseInstruction $ unlines i
    getValue k m = fromMaybe [] $ Map.lookup k m
    countPaths [] c = c
    countPaths (x : xs) c = countPaths (xs ++ getValue x input) (if x == "out" then c + 1 else c)

part2 :: [String] -> Int
part2 i = res
  where
    input = Map.fromList $ U.runParser parseInstruction $ unlines i
    getValue k m = fromMaybe [] $ Map.lookup k m
    res =
      memoFix
        ( \rec (fft, dac, node) ->
            if node == "out"
              then if fft && dac then 1 else 0
              else
                sum $
                  (\n -> rec (fft || node == "fft", dac || node == "dac", n))
                    <$> fromJust (Map.lookup node input)
        )
        (False, False, "svr")

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines