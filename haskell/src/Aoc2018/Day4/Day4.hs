{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Aoc2018.Day4.Day4 where

import           Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as M
import           Data.Ix (range)
import           Data.List (sort)
import           Data.Maybe (listToMaybe)
import           Data.Vector ((//))
import qualified Data.Vector as V
import           Text.ParserCombinators.ReadP
import           Utils

type GuardId = Int
data Transition = FallsAsleep | WakesUp | BeginShift GuardId deriving (Eq, Ord, Show)
data Timestamp = Timestamp { _year :: Int
                           , _month :: Int
                           , _day :: Int
                           , _hour :: Int
                           , _minute :: Int
                           } deriving (Eq, Ord, Show)
data Event = Event Timestamp Transition deriving (Eq, Ord, Show)

parseEvent :: ReadP Event
parseEvent = Event <$> parseTimestamp <*> (char ' ' *> parseTransition)
  where parseTimestamp = between (char '[') (char ']') $ do
          year <- parseInt
          month <- char '-' >> parseInt
          day <- char '-' >> parseInt
          hour <- char ' ' >> parseInt
          min' <- char ':' >> parseInt
          return $ Timestamp year month day hour min'
        parseTransition =
            (string "falls asleep" >> pure FallsAsleep) <|>
            (string "wakes up" >> pure WakesUp) <|>
            (string "Guard #" >> (BeginShift <$> parseInt) <* string " begins shift")

parseEvents :: [String] -> Maybe [Event]
parseEvents input = do
  events <- traverse (listToMaybe . map fst . readP_to_S parseEvent) input
  return $ sort events

reduceEvents :: [Event] -> [(GuardId, Int, Int)]
reduceEvents = go (Nothing, Nothing, [])
  where go (_, _, res) [] = reverse res
        go (guard, asleep, res) (Event time trans : evs) =
          let accum = case trans of
                BeginShift id' -> (Just id', Nothing, res)
                FallsAsleep -> (guard, Just (_minute time), res)
                WakesUp ->
                  let r = (,,) <$> guard <*> asleep <*> pure (_minute time)
                  in (guard, Nothing, maybe res (: res) r)
          in go accum evs

type SleepMap = M.IntMap (V.Vector Int)

groupShifts :: [(GuardId, Int, Int)] -> SleepMap
groupShifts = foldr (join . create) M.empty
  where create (id', sleep, awake) = M.singleton id' (vect (sleep, awake - 1))
        vect r = V.replicate 60 0 // map (, 1) (range r)
        join = M.unionWith $ V.zipWith (+)

puzzle :: IO ()
puzzle = parseEvents <$> readLines "./src/Aoc2018/Day4/input.txt" >>= \case
    Nothing -> putStrLn "No parse"
    Just events -> do
      let sleepMap = groupShifts $ reduceEvents events
          sleepAssoc = M.assocs sleepMap

      let (g1, s1) = maxBy (sum . snd) sleepAssoc
          (m1, _) = maxBy snd $ zip [0 ..] $ V.toList s1
      expect "part 1: " 84636 (g1 * m1)

      let (g2, s2) = maxBy (maximum . snd) sleepAssoc
          (m2, _) = maxBy snd $ zip [0 ..] $ V.toList s2
      expect "part 2: " 91679 (g2 * m2)
