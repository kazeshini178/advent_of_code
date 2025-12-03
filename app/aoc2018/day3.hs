module AOC2018.Day3 where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.String
import Utils qualified as U

data Claim = Fabric Int (Int, Int) (Int, Int)

claimParser :: Parser Claim
claimParser = do
  num <- char '#' *> number
  x <- string " @ " *> number
  y <- char ',' *> number
  w <- string ": " *> number
  h <- char 'x' *> number
  return $ Fabric num (x, y) (w, h)
  where
    number = read <$> many1 digit

claimParser' :: Parser Claim
claimParser' =
  Fabric
    <$> (char '#' *> number)
    <*> ((,) <$> (string " @ " *> number) <*> (char ',' *> number))
    <*> ((,) <$> (string ": " *> number) <*> (char 'x' *> number))
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 a = overlapCount
  where
    claims = concatMap (inches . U.runParser claimParser) a
    inches (Fabric _ (x, y) (w, h)) = [(nx, ny) | nx <- [x .. x + w - 1], ny <- [y .. y + h - 1]]
    overlaps = Map.filter (> 1) . List.foldl' (\p c -> Map.insertWith (+) c 1 p) Map.empty $ claims
    overlapCount = Map.size overlaps

part2 :: [String] -> Int
part2 a = perfectClaim claimsById
  where
    claimsById = inches . U.runParser claimParser <$> a
    claims = concat claimsById
    inches (Fabric cId (x, y) (w, h)) = [(cId, (nx, ny)) | nx <- [x .. x + w - 1], ny <- [y .. y + h - 1]]
    overlaps = Map.filter (> 1) . List.foldl' (\p (_, c) -> Map.insertWith (+) c 1 p) Map.empty $ claims
    perfectClaim [] = 0
    perfectClaim (g : gs) = if hasOverlap then perfectClaim gs else cid
      where
        (cid, _) = head g
        hasOverlap = any (\(_, coord) -> coord `Map.member` overlaps) g

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines