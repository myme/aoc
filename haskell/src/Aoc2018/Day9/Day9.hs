{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Aoc2018.Day9.Day9 where

import qualified Data.IntMap.Strict as M
import           Data.Monoid
import           Lens.Micro.Platform
import           Text.ParserCombinators.ReadP
import           Utils

parseInput :: String -> Maybe (Int, Int)
parseInput = parse $ (,)
  <$> parseInt
  <*> (string " players; last marble is worth " *> parseInt)
  <* string " points"

type Players = Int
type Marbles = ([Int], [Int]) -- ^ (rwd, fwd)
data Game = G { _score :: M.IntMap Int
              , _players :: Players
              , _marble :: Int
              , _marbles :: Marbles
              } deriving Show

makeLenses ''Game

initialGame :: Players -> Game
initialGame p = G (M.fromList $ map (,0) [1 .. p]) p 1 ([0], [])

repeatEndo :: Int -> (a -> a) -> (a -> a)
repeatEndo = appEndo . foldMap Endo ... replicate

cw :: Marbles -> Marbles
cw ms = case ms of
  ([], []) -> ms
  (r, []) -> let (c:f) = reverse r in ([c], f)
  (r, c:f) -> (c:r, f)

ccw :: Marbles -> Marbles
ccw ms = case ms of
  ([], []) -> ms
  ([], _) -> undefined
  ([_], []) -> ms
  ([c], f) -> (reverse (c:f), [])
  (c:r, f) -> (r, c:f)

pop :: Marbles -> (Int, Marbles)
pop ms = case ms of
  ([], _) -> undefined
  ([c], []) -> (c, ([], []))
  ([c], c':f) -> (c, ([c'], f))
  (_:_, []) -> undefined
  (c:r, c':f) -> (c, (c':r, f))

insert :: Int -> Marbles -> Marbles
insert m ms = case cw ms of
  ([], []) -> ([m], [])
  (r, f) -> (m:r, f)

step :: Game -> Game
step g | not23 = let
           g' = g & (marbles %~ insert m) & (marble %~ (+1))
           in g'
       | otherwise = let
           (c, ms) = g ^. marbles & repeatEndo 7 ccw & pop
           g' = g & marbles .~ ms
           s = c + m
           in g' & (marble %~ (+1)) & (score %~ M.adjust (+s) p)
  where
    m = g ^. marble -- current marble
    p = m `mod` g ^. players -- player
    not23 = m `mod` 23 /= 0

type TotalMarbles = Int
runGame :: Players -> TotalMarbles -> Game
runGame p m = repeatEndo m step $ initialGame p

puzzle :: IO ()
puzzle =
  parseInput <$> readFile "./src/Aoc2018/Day9/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just (ps, ms) -> do
      let highscore = maximum . map snd . M.toList . view score
      expect "part 1: " 398730 (highscore $ runGame ps ms)
      expect "part 2: " 3349635509 (highscore $ runGame ps (ms * 100))
