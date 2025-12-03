module Main where

import AOC2015.Module qualified as AOC2015
import AOC2018.Module qualified as AOC2018
import AOC2023.Module qualified as AOC2023
import AOC2024.Module qualified as AOC2024
import AOC2025.Module qualified as AOC2025
import Control.Applicative (optional)
import Data.Functor (($>))
import Text.Parsec (char, choice, digit, endOfLine, many1, string, try, parse)
import Text.Parsec.String (Parser)
import Text.Printf (printf)
import Utils (AocDate (All, One), Challenge (..), Result (..))

parseChallenge :: Parser Challenge
parseChallenge = Challenge <$> aocDate <* sep <*> aocDate <* optional endOfLine
  where
    number = read <$> many1 digit
    sep =
      choice
        [ try $ string ", ",
          try $ string ","
        ]
    aocDate =
      choice
        [ try $ One <$> number,
          try $ char '*' $> All
        ]

runModule :: Challenge -> IO Result
runModule (Challenge All All) = pure (Result 1 1)
runModule (Challenge year All) = head $ map (runModule . Challenge year . One) [1 .. 25]
runModule (Challenge All day) = head $ map (\year -> runModule (Challenge (One year) day)) [2015 .. 2025] -- TODO: Implement Display for all results
runModule (Challenge (One year) (One day)) = case year of
  2015 -> AOC2015.runChallenge day
  2016 -> error "Year (%i) not completed yet..." year
  2017 -> error "Year (%i) not completed yet..." year
  2018 -> AOC2018.runChallenge day
  2019 -> error "Year (%i) not completed yet..." year
  2020 -> error "Year (%i) not completed yet..." year
  2021 -> error "Year (%i) not completed yet..." year
  2022 -> error "Year (%i) not completed yet..." year
  2023 -> AOC2023.runChallenge day
  2024 -> AOC2024.runChallenge day
  2025 -> AOC2025.runChallenge day
  _ -> error "Year (%i) does not exist in the challenge..." year

printResult :: Result -> IO ()
printResult (Result part1 part2) =
  printf "Part 1: %i \nPart 2: %i\n" part1 part2

processResult :: Challenge -> IO ()
processResult c = do
  result <- runModule c
  -- print c
  printResult result

main :: IO ()
main =
  do
    printf "Welcome to AOC, which challenge would you like to answer? \n(year, day)\n"
    input <- getLine
    let challenge = parse parseChallenge "" input
    either (printf "Error: %s" . show) processResult challenge
