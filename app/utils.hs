module Utils where

import qualified Data.Text  as T
import System.Directory.OsPath
import System.Directory.Internal
import Text.Parsec
import Text.Parsec.String

-- data Challenge year day = All
--   | Year year
--   | Day year day
data AocDate = All | One Int deriving (Show)
data Challenge = Challenge AocDate AocDate deriving (Show)
-- data Challenge = Challenge ALL
-- data Challenge = Challenge Int ALL
-- data Challenge = Challenge ALL Int


data Result = 
  Result Int Int 
  | Result' String Int
  | Result'' Int String
  deriving (Show)

getChallengeData :: Int -> Int -> IO T.Text
getChallengeData year day = 
  do
    fileExist <- doesFileExist (os fileName)
    T.pack
      <$> if fileExist
        then readFile fileName
        else error "File does not exist"
        -- else downloadChallengeData challengeDay
  where
    fileName = "./app/aoc"++show year++"/data/day" ++ show day ++ ".txt"


runParser :: Parser a -> String -> a
runParser parser input =
  case parse parser "" input of
    Left e -> error $ show e
    Right v -> v

-- sessionToken :: Maybe String
-- sessionToken = Nothing

-- -- downloadChallengeData :: Int -> IO String
-- -- downloadChallengeData challengeDay = ""

-- sessionToken = Just "Blah"