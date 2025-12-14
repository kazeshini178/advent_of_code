module AOC2025.Day12 where

import Data.Functor
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Debug.Trace
import Text.Parsec
  ( ParsecT,
    char,
    choice,
    digit,
    many1,
    newline,
    optional,
    string,
    try,
  )
import Text.Parsec.String (Parser)
import Utils qualified as U

data Present = Present [PresentPiece] deriving (Show)

data PresentPiece = Filled | Free deriving (Show)

data TreeArea = Area Grid PresentList deriving (Show)

type Grid = (Int, Int)

type PresentList = [Int]

parseInstruction :: Parser ([Present], [TreeArea])
parseInstruction =
  (,) <$> many1 (try presentParser) <*> many1 (try treeAreaParser)
  where
    presentParser = Present <$> (number *> char ':' *> newline *> presentlineParser <* newline) -- <* newline
    presentlineParser = do
      line1 <- many1 (choice (try <$> [char '#' $> Filled, char '.' $> Free])) <* newline
      line2 <- many1 (choice (try <$> [char '#' $> Filled, char '.' $> Free])) <* newline
      line3 <- many1 (choice (try <$> [char '#' $> Filled, char '.' $> Free])) <* newline
      pure $ concat [line1, line2, line3]
    treeAreaParser = Area <$> ((,) <$> (number <* char 'x') <*> (number <* string ": ")) <*> many1 (number <* optional (char ' ')) <* newline
    number :: ParsecT String () Identity Int = read <$> many1 digit

part1 :: [String] -> Int
part1 i = trace (show input) 0
  where
    input@(presents, areas) = U.runParser parseInstruction $ unlines i

part2 :: [String] -> Int
part2 i = 0

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines