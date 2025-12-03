module  AOC2018.Day4 where

import Data.List qualified as List
import Data.Map qualified as Map
import Chronos qualified as C
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String
import Data.Set
import Utils qualified as U
import Data.Text qualified as T

data Event
  = Shift C.Datetime Int
  | WakeUp C.Datetime
  | Sleep C.Datetime
  deriving(Show)

parseEvent :: Parser Event
parseEvent = do
  eventTime <-
    C.datetimeFromYmdhms
      <$> parsePrefixNumber "["
      <*> parsePrefixNumber "-"
      <*> parsePrefixNumber "-"
      <*> parsePrefixNumber " "
      <*> parsePrefixNumber ":"
      <*> (0 <$ string "] ")
  choice $ try <$> [parseGuard eventTime, parseWakeUp eventTime, parseAsleep eventTime]

parsePrefixNumber :: (Read b, Stream s m Char) => String -> ParsecT s u m b
parsePrefixNumber a = string a *> number
  where
    number = read <$> many1 digit

parseGuard :: C.Datetime -> Parser Event
parseGuard e = do
  gId <- parsePrefixNumber "Guard #"
  pure $ Shift e gId

parseWakeUp :: C.Datetime -> Parser Event
parseWakeUp e = do
  _ <- string "wakes up"
  pure $ WakeUp e

parseAsleep :: C.Datetime -> Parser Event
parseAsleep e = do
  _ <- string "falls asleep"
  pure $ Sleep e

part1 :: [String] -> Int
part1 a = sleepistGuard * sleepistMinute sleepistGuard
  where
    events = List.sortOn byDate $ U.runParser parseEvent <$> a
    grouped = List.foldl' byShift []
    byDate (Shift d _) = d
    byDate (WakeUp d) = d
    byDate (Sleep d) = d
    byShift p e = case e of
      (Shift _ i) -> p ++ [(i, [])]
      (WakeUp (C.Datetime _ (C.TimeOfDay _ m _))) -> addEventChange p m '.'
      (Sleep (C.Datetime _ (C.TimeOfDay _ m _))) -> addEventChange p m '#'
    addEventChange p m s = np ++ [(i, nel)]
      where
        (i, el) = List.last p
        nel = el ++ [(m, s)]
        np = List.delete (i, el) p
    guardSchedules = convertEventsToSchedule <$> grouped events
    convertEventsToSchedule (i, e) = (i, buildSchedule e 0 '.' [])
    buildSchedule es m c s = if m == 60 then s else buildSchedule es (m + 1) state (s ++ [state])
      where
        state = fromMaybe c (lookup m es)
    sleepistGuard = gaurd
      where
        (gaurd, _) = Map.foldrWithKey (\i c (pi, pc) -> if c > pc then (i, c) else (pi, pc)) (0, 0) guardSleepCount
        guardSleepCount = List.foldl' (\m (k, s) -> Map.insertWith (+) k (countSleep s) m) Map.empty guardSchedules
        countSleep = length . List.filter (== '#')
    sleepistMinute guard = fromMaybe (-1) (List.elemIndex (maximum totalSleepPerMinute) totalSleepPerMinute)
      where
        totalSleepPerMinute = List.map sum . List.transpose $ List.map (\s -> if s == '.' then 0 :: Int else 1) <$> schedules
        schedules = List.map snd . List.filter (\(g, _) -> g == guard) $ guardSchedules

part2 :: [String] -> Int
part2 a = sleepistMinute * guardAsleepAt sleepistMinute
  where
    events = List.sortOn byDate $ U.runParser parseEvent <$> a
    grouped = List.foldl' byShift []
    byDate (Shift d _) = d
    byDate (WakeUp d) = d
    byDate (Sleep d) = d
    byShift p e = case e of
      (Shift _ i) -> p ++ [(i, [])]
      (WakeUp (C.Datetime _ (C.TimeOfDay _ m _))) -> addEventChange p m '.'
      (Sleep (C.Datetime _ (C.TimeOfDay _ m _))) -> addEventChange p m '#'
    addEventChange p m s = np ++ [(i, nel)]
      where
        (i, el) = List.last p
        nel = el ++ [(m, s)]
        np = List.delete (i, el) p
    guardSchedules = convertEventsToSchedule <$> grouped events
    convertEventsToSchedule (i, e) = (i, buildSchedule e 0 '.' [])
    buildSchedule es m c s = if m == 60 then s else buildSchedule es (m + 1) state s ++ [state]
      where
        state = fromMaybe c (lookup m es)
    sleepistMinute = fromMaybe (-1) (List.elemIndex (maximum totalSleepPerMinute) totalSleepPerMinute)
      where
        totalSleepPerMinute= reverse . List.map sum . List.transpose $ List.map (\ s -> if s == '.' then 0::Int else 1) <$> schedules
        schedules = List.map snd guardSchedules
    guardAsleepAt m = 
      fst $ List.foldl' (\p c -> if snd p < snd c then c else p) (0,0) 
      $ List.map (\g -> (head g, length g)) 
      $ List.group 
      $ List.sort 
      $ fst <$> List.filter (\(_, e) -> e !! m == '#') guardSchedules

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines